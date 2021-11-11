{-# LANGUAGE OverloadedLists #-}

module Vehicle.Language.Normalise
  ( NormError
  , normalise
  , normaliseInternal
  ) where

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.State (MonadState(..), evalStateT, gets, modify)
import Control.Monad.Except (MonadError, ExceptT, runExceptT)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Data.List.Split (chunksOf)

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print (prettySimple, prettyVerbose)

-- |Run a function in 'MonadNorm'.
normalise :: Norm a => a -> ExceptT NormError Logger a
normalise x = do
  logDebug "Beginning normalisation"
  result <- evalStateT (nf x) mempty
  logDebug "Finished normalisation\n"
  return result

-- |Should only be run when we know there can be no internal errors
normaliseInternal :: Norm a => a -> a
normaliseInternal x = fromRight
  (developerError "Error whilst performing supposedly safe normalisation")
  (discardLogger $ runExceptT (normalise x))

--------------------------------------------------------------------------------
-- Setup

type DeclCtx = M.Map Identifier CheckedExpr

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m =
  ( MonadError NormError m
  , MonadLogger m
  , MonadState DeclCtx m
  )

-- |Errors thrown during normalisation
newtype NormError
  = EmptyQuantifierDomain Provenance

instance MeaningfulError NormError where
  details (EmptyQuantifierDomain p) = UError $ UserError
    { problem    = "Quantifying over an empty domain"
    , provenance = p
    , fix        = "Check your definition of the domain"
    }

pattern ETrue :: ann -> Expr binder var ann
pattern ETrue ann = Literal ann (LBool True)

pattern EFalse :: ann -> Expr binder var ann
pattern EFalse ann = Literal ann (LBool False)

argHead :: CheckedArg -> CheckedExpr
argHead = exprHead . argExpr

--------------------------------------------------------------------------------
-- Debug functions

showEntry :: MonadNorm m => CheckedExpr -> m CheckedExpr
showEntry e = do
  logDebug ("norm-entry " <> prettySimple e)
  incrCallDepth
  return e

showExit :: MonadNorm m => CheckedExpr -> m CheckedExpr -> m CheckedExpr
showExit old mNew = do
  new <- mNew
  decrCallDepth
  when (old /= new) $
    logDebug ("normalising" <+> prettySimple old)
  logDebug ("norm-exit " <+> prettySimple new)
  return new

--------------------------------------------------------------------------------
-- Normalisation algorithm

-- |Class for the various normalisation functions.
-- Invariant is that everything in the context is fully normalised
class Norm vf where
  nf :: MonadNorm m => vf -> m vf

instance Norm CheckedProg where
  nf (Main decls)= Main <$> traverse nf decls

instance Norm CheckedDecl where
  nf = \case
    DeclNetw ann ident typ      -> DeclNetw ann ident <$> nf typ
    DeclData ann ident typ      -> DeclData ann ident <$> nf typ

    DefFun   ann ident typ expr -> do
      typ'  <- nf typ
      expr' <- nf expr
      modify (M.insert ident expr')
      return $ DefFun ann ident typ' expr'

instance Norm CheckedExpr where
  nf e = showExit e $ do
    e' <- showEntry e
    case e' of
      Type{}      -> return e
      Hole{}      -> return e
      Literal{}   -> return e
      Builtin{}   -> return e
      Meta{}      -> developerError "All metas should have been solved before normalisation"

      PrimDict tc         -> nf tc
      Seq ann exprs       -> Seq ann <$> traverse nf exprs
      Lam ann binder expr -> Lam ann <$> nf binder <*> nf expr
      Pi ann binder body  -> Pi ann <$> nf binder <*> nf body

      Ann _ann expr _typ  -> nf expr

      Var _ (Bound _)     -> return e
      Var _ (Free ident)  -> gets (fromMaybe e . M.lookup ident)

      Let ann letValue binder letBody -> do
        letValue' <- nf letValue
        binder'  <- nf binder
        letBody' <- nf letBody
        return $ Let ann letValue' binder' letBody'
        -- TODO renable let normalisation once we get left lifting re-enabled
        {-
        let letBodyWithSubstitution = substInto letValue' letBody'
        nf letBodyWithSubstitution
        -}

      eApp@(App ann _ _) -> let (fun, args) = toHead eApp in do
        nFun  <- nf fun
        -- TODO temporary hack please remove in future once we actually have computational
        -- behaviour in implicit/instance arguments
        nArgs <- traverse (\arg -> if visibilityOf arg == Explicit then nf arg else return arg) args
        nfApp ann nFun nArgs

instance Norm CheckedBinder where
  nf = traverseBinderType nf

instance Norm CheckedArg where
  nf (ExplicitArg ann e) = ExplicitArg ann <$> nf e
  nf arg@Arg{}           = return arg

--------------------------------------------------------------------------------
-- Application

nfApp :: MonadNorm m => CheckedAnn -> CheckedExpr -> [CheckedArg] -> m CheckedExpr
nfApp _ann (Lam _ _ funcBody) (arg : _) = nf (substInto (argExpr arg) funcBody)
nfApp ann  fun@(Builtin _ op) args      = do
  let e = normAppList ann fun args
  case (op, args) of
    -- Equality
    (Equality eq, [tElem, tRes, tc, arg1, arg2]) ->
      case eq of
        Eq  -> fromMaybe (return e) (nfEq ann tElem tRes tc (argExpr arg1) (argExpr arg2))
        Neq -> case (argHead arg1, argHead arg2) of
          --(ETrue  _, _)          -> normApp $ composeApp ann (Builtin ann Not) [_t2, _, arg2]
          (EFalse _, _)              -> return $ argExpr arg2
          (LitNat _ m, LitNat _ n) -> return $ mkBool ann (argExpr tRes) (m /= n)
          (LitInt _ i, LitInt _ j) -> return $ mkBool ann (argExpr tRes) (i /= j)
          (LitRat _ x, LitRat _ y) -> return $ mkBool ann (argExpr tRes) (x /= y)
          _                          -> return e

    -- Not
    (Not, [t, tc, arg]) ->
      fromMaybe (return e) (nfNot ann t tc arg)

    -- Binary boolean operations
    (BooleanOp2 op2, [t, tc, arg1, arg2]) -> case (op2, argHead arg1, argHead arg2) of
      -- TODO implement associativity rules?
      -- See https://github.com/wenkokke/vehicle/issues/2
      (And, ETrue  _, _)        -> return $ argExpr arg2
      (And, _,        ETrue  _) -> return $ argExpr arg1
      (And, EFalse _, _)        -> return $ mkBool ann (argExpr t) False
      (And, _,        EFalse _) -> return $ mkBool ann (argExpr t) False

      (Or, ETrue  _, _)        -> return $ mkBool ann (argExpr t) True
      (Or, EFalse _, _)        -> return $ argExpr arg2
      (Or, _,        ETrue  _) -> return $ mkBool ann (argExpr t) True
      (Or, _,        EFalse _) -> return $ argExpr arg1

      (Impl, ETrue  _, _)        -> return $ argExpr arg2
      (Impl, EFalse _, _)        -> return $ mkBool ann (argExpr t) True
      (Impl, _,        ETrue  _) -> return $ mkBool ann (argExpr t) True
      (Impl, _,        EFalse _) -> return $ App ann (Builtin ann Not) [t, tc, arg2]

      _ -> return e

    -- If
    (If, [_tRes, arg1, arg2, arg3]) -> case argHead arg1 of
      ETrue  _ -> return $ argExpr arg2
      EFalse _ -> return $ argExpr arg3
      _        -> return e

    -- Le
    (Order Le, [_t1, tRes, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      (LitInt _ i, LitInt _ j) -> return $ mkBool ann (argExpr tRes) (i <= j)
      (LitRat _ x, LitRat _ y) -> return $ mkBool ann (argExpr tRes) (x <= y)
      _                          -> return e

    -- Lt
    (Order Lt, [_t1, tRes, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      (LitInt _ i, LitInt _ j) -> return $ mkBool ann (argExpr tRes) (i < j)
      (LitRat _ x, LitRat _ y) -> return $ mkBool ann (argExpr tRes) (x < y)
      _                          -> return e

    -- Binary numeric ops
    (NumericOp2 op2, [_t, _tc, arg1, arg2]) -> case (op2, argHead arg1, argHead arg2) of
      -- TODO implement zero/identity/associativity rules?
      (Add, LitNat _ m, LitNat _ n) -> return $ mkNat ann (m + n)
      (Add, LitInt _ i, LitInt _ j) -> return $ mkInt ann (i + j)
      (Add, LitRat _ x, LitRat _ y) -> return $ mkRat ann (x + y)

      (Sub, LitInt _ i, LitInt _ j) -> return $ mkInt ann (i - j)
      (Sub, LitRat _ x, LitRat _ y) -> return $ mkRat ann (x - y)

      (Mul, LitInt _ i, LitInt _ j) -> return $ mkInt ann (i * j)
      (Mul, LitRat _ x, LitRat _ y) -> return $ mkRat ann (x * y)

      (Div, LitRat _ x, LitRat _ y) -> return $ mkRat ann (x / y)

      _                          -> return e

    -- Negation
    (Neg, [t, tc, arg]) -> fromMaybe (return e) (nfNeg ann t tc arg)

    -- Cons
    (Cons, [tElem, x, cont]) -> case argHead cont of
      Seq _ xs -> return $ mkSeq ann (argExpr tElem) (mkListType ann (argExpr tElem)) (argExpr x : xs)
      _        -> return e

    -- Lookup
    (At, [_tElem, _tDims, cont, index]) ->
      case (argHead cont, argHead index) of
        (Seq _ es, LitNat _ i) -> return $ es !! fromIntegral i
        _ -> return e --(xs      , i) -> case (decomposeExplicitApp xs, i) of
        --((Builtin _ Cons, [x, _]), LitInt _ 0) -> return $ argExpr x
        --((Builtin _ Cons, [_, _, _, x, xs']), LitInt _ i) -> Op2 EAt es (LitInt ann3 (i - 1)) ann ann1 ann2 pos
        --_                                     -> return e

    -- Map
    (Map, [tElem, tRes, fn, cont]) ->
      fromMaybe (return e) (nfMap ann tElem tRes fn cont)

    -- TODO distribute over cons

    -- Fold
    (Fold, [_tElem, _tCont, _tRes, _tc, foldOp, unit, cont]) -> case argHead cont of
      Seq _ xs -> nf $ foldr (\x body -> normApp ann (argExpr foldOp) [ExplicitArg ann x, ExplicitArg ann body]) (argExpr unit) xs
      _        -> return e
    -- TODO distribute over cons

    -- Quantifiers
    (Quant q, [_tElem, lam]) ->
      fromMaybe (return e) (nfQuantifier ann q lam)

    -- Quantifiers over containers
    (QuantIn q, [tElem, tCont, tRes, tc, lam, cont]) ->
      fromMaybe (return e) (nfQuantifierIn ann q tElem tCont tRes tc lam cont)

    -- Fall-through case
    _ -> return e

nfApp ann fun args = return $ normAppList ann fun args

--------------------------------------------------------------------------------
-- Normalising equality

nfEq :: MonadNorm m
     => CheckedAnn
     -> CheckedArg
     -> CheckedArg
     -> CheckedArg
     -> CheckedExpr
     -> CheckedExpr
     -> Maybe (m CheckedExpr)
nfEq ann _tEq tRes tc e1 e2 = case (toHead e1, toHead e2) of
  --(EFalse _,  _)         -> normApp $ composeApp ann (Builtin ann op, [tElem, _, e2])
  ((LitNat _ m, _),         (LitInt _ n, _))     -> Just $ return $ mkBool ann (argExpr tRes) (m == n)
  ((LitInt _ i, _),         (LitInt _ j, _))     -> Just $ return $ mkBool ann (argExpr tRes) (i == j)
  ((LitRat _ x, _),         (LitRat _ y, _))     -> Just $ return $ mkBool ann (argExpr tRes) (x == y)
  ((Seq _ xs, [tElem,_,_]), (Seq _ ys, [_,_,_])) -> Just $
    if length xs /= length ys then
      return $ mkBool ann (argExpr tRes) False
    else
      nf $ foldr (\(x,y) res -> mkBoolOp2' And ann tRes tc (mkEq' ann tElem tRes tc x y) res) (mkBool ann (argExpr tRes) True) (zip xs ys)
  _                     -> Nothing
  -- TODO implement reflexive rules?

--------------------------------------------------------------------------------
-- Normalising quantification over types

nfQuantifier :: MonadNorm m
             => CheckedAnn
             -> Quantifier
             -> CheckedArg -- The quantified lambda expression
             -> Maybe (m CheckedExpr)
nfQuantifier ann q lam = case argHead lam of
  Lam _ann binder body -> case toHead (typeOf binder) of
    -- If we have a tensor instead quantify over each individual element, and then substitute
    -- in a Seq construct with those elements in.
    (BuiltinContainerType _ Tensor, [tElemArg, tDimsArg]) ->
      case getDimensions (argHead tDimsArg) of
        Nothing -> Nothing
        Just dims -> Just $ do
          let tElem = argExpr tElemArg

          -- Calculate the dimensions of the tensor
          let tensorSize = product dims

          -- Use the list monad to create a nested list of all possible indices into the tensor
          let allIndices = traverse (\dim -> [0..dim-1]) dims
          -- Generate the corresponding names from the indices
          let allNames   = map (mkNameWithIndices (nameOf binder)) (reverse allIndices)

          -- Generate a list of variables, one for each index
          let allExprs   = map (\i -> Var ann (Bound i)) (reverse [0..tensorSize-1])
          -- Construct the corresponding nested tensor expression
          let tensor     = makeTensor ann tElem dims allExprs
          -- We're introducing `tensorSize` new binder so lift the indices in the body accordingly
          let body1      = liftDBIndices tensorSize body
          -- Substitute throught the tensor expression for the old top-level binder
          body2 <- nf $ substIntoAtLevel tensorSize tensor body1

          -- Generate a expression prepended with `tensorSize` quantifiers
          return $ foldl (makeQuantifier ann q tElem) body2 allNames

    _ -> Nothing
  _ -> Nothing

makeQuantifier :: CheckedAnn -> Quantifier -> CheckedExpr -> CheckedExpr -> Name -> CheckedExpr
makeQuantifier ann q tElem body name =
  App ann (Builtin ann (Quant q))
    [ExplicitArg ann (Lam ann (ExplicitBinder ann name tElem) body)]

makeTensor :: CheckedAnn -> CheckedExpr -> [Int] -> [CheckedExpr] -> CheckedExpr
makeTensor ann tElem dims exprs = assert (product dims == length exprs) (go dims exprs)
  where
    mkTensorSeq :: [Int] -> [CheckedExpr] -> CheckedExpr
    mkTensorSeq ds xs = mkSeq ann tElem (mkTensorType ann tElem ds) xs

    go []       [] = mkTensorSeq []       []
    go [d]      es = mkTensorSeq [d]      es
    go (d : ds) es = mkTensorSeq (d : ds) (map (go ds) (chunksOf (product ds) es))
    go []  (_ : _) = developerError "Found inhabitants of the empty dimension! Woo!"

getDimensions :: CheckedExpr -> Maybe [Int]
getDimensions (Seq _ xs) = traverse getDimension xs
getDimensions _          = Nothing

getDimension :: CheckedExpr -> Maybe Int
getDimension e = case exprHead e of
  (Literal _ (LNat i)) -> Just i
  _                    -> Nothing

--------------------------------------------------------------------------------
-- Normalising quantification over lists

-- |Elaborate quantification over the members of a container type.
-- Expands e.g. `every x in list . y` to `fold and true (map (\x -> y) list)`
nfQuantifierIn :: MonadNorm m
               => CheckedAnn
               -> Quantifier
               -> CheckedArg -- tElem
               -> CheckedArg -- tCont
               -> CheckedArg -- tRes
               -> CheckedArg -- IsContainer tElem tCont
               -> CheckedArg
               -> CheckedArg
               -> Maybe (m CheckedExpr)
nfQuantifierIn ann q tElem tCont tRes _tc lam container = do
  let (bop, unit) = quantImplementation q
  let isTruthTC = InstanceArg ann (PrimDict (mkIsTruth ann (argExpr tRes)))
  let bopExpr = App ann (Builtin ann (BooleanOp2 bop)) [tRes, isTruthTC]
  let unitExpr = mkLiteral' ann (LBool unit) tRes isTruthTC
  let tResCont = mapArgExpr (substContainerType tRes) tCont
  let tResContTC = PrimDict $ mkIsContainer ann (argExpr tRes) (argExpr tResCont)
  let mappedContainer = mkMap' ann tElem tRes lam container
  let foldedContainer = mkFold' ann tRes tResCont tRes (InstanceArg ann tResContTC) bopExpr unitExpr mappedContainer
  Just (nf foldedContainer)

quantImplementation :: Quantifier -> (BooleanOp2, Bool)
quantImplementation All = (And, True)
quantImplementation Any = (Or,  False)

--------------------------------------------------------------------------------
-- Normalising not

-- TODO implement idempotence rules?
nfNot :: MonadNorm m
      => CheckedAnn
      -> CheckedArg
      -> CheckedArg
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfNot ann t tc arg = case toHead (argExpr arg) of
  (ETrue  _, [_, _])            -> Just $ return $ mkBool ann (argExpr t) False
  (EFalse _, [_, _])            -> Just $ return $ mkBool ann (argExpr t) True

  -- Negation juggling
  (Builtin _ (Quant q),         [lam])        -> Just $ nfNotQuantifier ann t tc q (argExpr lam)
  (Builtin _ (BooleanOp2 Impl), [_,_,e1,e2])  -> Just $ nf $ mkBoolOp2' And ann t tc (argExpr e1) (neg (argExpr e2))
  --(Builtin _ And,  [_,_,e1,e2])  -> Just $ nf $ mkOr'  ann t tc (neg $ argExpr e1) (neg $ argExpr e2)
  (Builtin _ (BooleanOp2 Or),   [_,_,e1,e2])  -> Just $ nf $ mkBoolOp2' And ann t tc (neg $ argExpr e1) (neg $ argExpr e2)
  (BuiltinOrder    ann2 o,      args)         -> Just $ return $ normAppList ann (BuiltinOrder    ann2 (negateOrder o)) args
  (BuiltinEquality ann2 eq,     args)         -> Just $ return $ normAppList ann (BuiltinEquality ann2 (negateEquality eq)) args
  _                                           -> Nothing
  where
    neg = mkNot' ann t tc

nfNotQuantifier :: MonadNorm m
                => CheckedAnn
                -> CheckedArg
                -> CheckedArg
                -> Quantifier
                -> CheckedExpr
                -> m CheckedExpr
nfNotQuantifier ann t tc q (Lam lAnn binder body) = do
  notBody <- nf $ App ann (Builtin ann Not) [t, tc, ExplicitArg lAnn body]
  let notLam = ExplicitArg ann (Lam lAnn binder notBody)
  return $ App ann (Builtin ann (Quant (negateQ q))) [notLam]
nfNotQuantifier _ _ _ _ e = developerError $
  "Malformed quantifier, was expecting a Lam but found" <+> prettyVerbose e

negateQ :: Quantifier -> Quantifier
negateQ Any = All
negateQ All = Any

negateOrder :: Order -> Order
negateOrder Le = Gt
negateOrder Lt = Ge
negateOrder Ge = Lt
negateOrder Gt = Le

negateEquality :: Equality -> Equality
negateEquality Eq = Neq
negateEquality Neq = Eq

-----------------------------------------------------------------------------
-- Normalising negation

nfNeg :: MonadNorm m
      => CheckedAnn
      -> CheckedArg
      -> CheckedArg
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfNeg ann _t _tc arg = case argHead arg of
  (LitInt _ x) -> Just $ return $ mkInt ann (- x)
  (LitRat _ x) -> Just $ return $ mkRat ann (- x)
  _            -> Nothing

-----------------------------------------------------------------------------
-- Normalising map

nfMap :: MonadNorm m
      => CheckedAnn
      -> CheckedArg
      -> CheckedArg
      -> CheckedArg
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfMap ann _tFrom tTo fun arg = case argHead arg of
  Seq _ xs -> Just $ do
    ys <- traverse (\x -> nf $ normApp ann (argExpr fun) [ExplicitArg ann x]) xs
    let tElem = argExpr tTo
    let tCont = mkListType ann tElem
    return $ mkSeq ann tElem tCont ys
  _        -> Nothing
