module Vehicle.Compile.Normalise
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
import Vehicle.Compile.Error
import Vehicle.Language.AST
import Vehicle.Language.Print (prettySimple)

-- |Run a function in 'MonadNorm'.
normalise :: (AsNormError e, Norm a) => a -> ExceptT e Logger a
normalise x = do
  logDebug "Beginning normalisation"
  result <- evalStateT (nf x) mempty
  logDebug "Finished normalisation\n"
  return result

-- |Should only be run when we know there can be no internal errors
normaliseInternal :: forall a . Norm a => a -> a
normaliseInternal x = fromRight
  (developerError "Error whilst performing supposedly safe normalisation")
  (discardLogger $ runExceptT (normalise x :: ExceptT CompileError Logger a))

--------------------------------------------------------------------------------
-- Setup

type DeclCtx = M.Map Identifier CheckedExpr

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm e m =
  ( AsNormError e
  , MonadError e m
  , MonadLogger m
  , MonadState DeclCtx m
  )

pattern ETrue :: ann -> Expr binder var ann
pattern ETrue ann = Literal ann (LBool True)

pattern EFalse :: ann -> Expr binder var ann
pattern EFalse ann = Literal ann (LBool False)

argHead :: CheckedArg -> CheckedExpr
argHead = exprHead . argExpr

--------------------------------------------------------------------------------
-- Debug functions

showEntry :: MonadNorm e m => CheckedExpr -> m CheckedExpr
showEntry e = do
  logDebug ("norm-entry " <> prettySimple e)
  incrCallDepth
  return e

showExit :: MonadNorm e m => CheckedExpr -> m CheckedExpr -> m CheckedExpr
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
  nf :: MonadNorm e m => vf -> m vf

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

nfApp :: MonadNorm e m => CheckedAnn -> CheckedExpr -> [CheckedArg] -> m CheckedExpr
nfApp _ann (Lam _ _ funcBody) (arg : _) = nf (substInto (argExpr arg) funcBody)
nfApp ann  fun@(Builtin _ op) args      = do
  let e = normAppList ann fun args
  case (op, args) of
    -- Equality
    (Equality eq, [tElem, tRes, tc, arg1, arg2]) ->
      fromMaybe (return e) (nfEq eq ann tElem tRes tc (argExpr arg1) (argExpr arg2))

    -- Not
    (Not, [t, tc, arg]) ->
      fromMaybe (return e) (nfNot ann (argExpr t) (argExpr tc) arg)

    -- Binary boolean operations
    (BooleanOp2 op2, [t, tc, arg1, arg2]) -> case (op2, argHead arg1, argHead arg2) of
      -- TODO implement associativity rules?
      -- See https://github.com/wenkokke/vehicle/issues/2
      (And, ETrue  _, _)        -> return $ argExpr arg2
      (And, _,        ETrue  _) -> return $ argExpr arg1
      (And, EFalse _, _)        -> return $ BoolLiteralExpr ann (argExpr t) False
      (And, _,        EFalse _) -> return $ BoolLiteralExpr ann (argExpr t) False

      (Or, ETrue  _, _)        -> return $ BoolLiteralExpr ann (argExpr t) True
      (Or, EFalse _, _)        -> return $ argExpr arg2
      (Or, _,        ETrue  _) -> return $ BoolLiteralExpr ann (argExpr t) True
      (Or, _,        EFalse _) -> return $ argExpr arg1

      (Impl, ETrue  _, _)        -> return $ argExpr arg2
      (Impl, EFalse _, _)        -> return $ BoolLiteralExpr ann (argExpr t) True
      (Impl, _,        ETrue  _) -> return $ BoolLiteralExpr ann (argExpr t) True
      (Impl, _,        EFalse _) -> return $ App ann (Builtin ann Not) [t, tc, arg2]

      _ -> return e

    -- If
    (If, [_tRes, arg1, arg2, arg3]) -> case argHead arg1 of
      ETrue  _ -> return $ argExpr arg2
      EFalse _ -> return $ argExpr arg3
      _        -> return e

    -- Le
    (Order Le, [_t1, tRes, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      (LitInt _ i, LitInt _ j) -> return $ BoolLiteralExpr ann (argExpr tRes) (i <= j)
      (LitRat _ x, LitRat _ y) -> return $ BoolLiteralExpr ann (argExpr tRes) (x <= y)
      _                          -> return e

    -- Lt
    (Order Lt, [_t1, tRes, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      (LitInt _ i, LitInt _ j) -> return $ BoolLiteralExpr ann (argExpr tRes) (i < j)
      (LitRat _ x, LitRat _ y) -> return $ BoolLiteralExpr ann (argExpr tRes) (x < y)
      _                          -> return e

    -- Binary numeric ops
    (NumericOp2 op2, [t, _tc, arg1, arg2]) -> case (op2, argHead arg1, argHead arg2) of
      -- TODO implement zero/identity/associativity rules?
      (Add, LitNat _ m, LitNat _ n) -> return $ NatLiteralExpr ann (argExpr t) (m + n)
      (Add, LitInt _ i, LitInt _ j) -> return $ IntLiteralExpr ann (argExpr t) (i + j)
      (Add, LitRat _ x, LitRat _ y) -> return $ RatLiteralExpr ann (argExpr t) (x + y)

      (Sub, LitInt _ i, LitInt _ j) -> return $ IntLiteralExpr ann (argExpr t) (i - j)
      (Sub, LitRat _ x, LitRat _ y) -> return $ RatLiteralExpr ann (argExpr t) (x - y)

      (Mul, LitInt _ i, LitInt _ j) -> return $ IntLiteralExpr ann (argExpr t) (i * j)
      (Mul, LitRat _ x, LitRat _ y) -> return $ RatLiteralExpr ann (argExpr t) (x * y)

      (Div, LitRat _ x, LitRat _ y) -> return $ RatLiteralExpr ann (argExpr t) (x / y)

      _                          -> return e

    -- Negation
    (Neg, [t, tc, arg]) -> fromMaybe (return e) (nfNeg ann t tc arg)

    -- Cons
    (Cons, [tElem, x, cont]) -> case argHead cont of
      Seq _ xs -> return $ SeqExpr ann (argExpr tElem) (ListType ann (argExpr tElem)) (argExpr x : xs)
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

nfEq :: MonadNorm e m
     => Equality
     -> CheckedAnn
     -> CheckedArg
     -> CheckedArg
     -> CheckedArg
     -> CheckedExpr
     -> CheckedExpr
     -> Maybe (m CheckedExpr)
nfEq eq ann _tEq tRes _tc e1 e2 = case (e1, e2) of
  (NatLiteralExpr _ _ m, NatLiteralExpr _ _ n) -> Just $ return $ BoolLiteralExpr ann (argExpr tRes) (eqOp eq m n)
  (IntLiteralExpr _ _ i, IntLiteralExpr _ _ j) -> Just $ return $ BoolLiteralExpr ann (argExpr tRes) (eqOp eq i j)
  (RatLiteralExpr _ _ p, RatLiteralExpr _ _ q) -> Just $ return $ BoolLiteralExpr ann (argExpr tRes) (eqOp eq p q)
  (SeqExpr _ tElem _ xs, SeqExpr _ _ _ ys)     -> Just $
    if length xs /= length ys then
      return $ BoolLiteralExpr ann (argExpr tRes) False
    else
      let equalities = zipWith (\x y -> EqualityExpr eq ann tElem (argExpr tRes) (map (ExplicitArg ann) [x,y])) xs ys in
      nf $ booleanBigOp (logicOp eq) ann (argExpr tRes) equalities
  _ -> Nothing
  where
    eqOp :: Eq a => Equality -> (a -> a -> Bool)
    eqOp Eq  = (==)
    eqOp Neq = (/=)

    logicOp :: Equality -> BooleanOp2
    logicOp Eq  = And
    logicOp Neq = Or

  -- TODO implement reflexive rules?

--------------------------------------------------------------------------------
-- Normalising quantification over types

nfQuantifier :: MonadNorm e m
             => CheckedAnn
             -> Quantifier
             -> CheckedArg -- The quantified lambda expression
             -> Maybe (m CheckedExpr)
nfQuantifier ann q lam = case argHead lam of
  Lam _ann binder body -> case typeOf binder of
    -- If we have a tensor instead quantify over each individual element, and then substitute
    -- in a Seq construct with those elements in.
    (TensorType _ tElem tDims) ->
      case getDimensions (exprHead tDims) of
        Nothing -> Nothing
        Just dims -> Just $ do
          -- Calculate the dimensions of the tensor
          let tensorSize = product dims

          -- Use the list monad to create a nested list of all possible indices into the tensor
          let allIndices = traverse (\dim -> [0..dim-1]) dims
          -- Generate the corresponding names from the indices
          let allNames   = map (mkNameWithIndices (getQuantifierSymbol binder)) (reverse allIndices)

          -- Generate a list of variables, one for each index
          let allExprs   = map (\i -> Var ann (Bound i)) (reverse [0..tensorSize-1])
          -- Construct the corresponding nested tensor expression
          let tensor     = makeTensorLit ann tElem dims allExprs
          -- We're introducing `tensorSize` new binder so lift the indices in the body accordingly
          let body1      = liftDBIndices tensorSize body
          -- Substitute throught the tensor expression for the old top-level binder
          body2 <- nf $ substIntoAtLevel tensorSize tensor body1

          -- Generate a expression prepended with `tensorSize` quantifiers
          return $ mkQuantifierSeq q ann (map Just allNames) tElem body2

    _ -> Nothing
  _ -> Nothing

makeTensorLit :: CheckedAnn -> CheckedExpr -> [Int] -> [CheckedExpr] -> CheckedExpr
makeTensorLit ann tElem dims exprs = assert (product dims == length exprs) (go dims exprs)
  where
    mkTensorSeq :: [Int] -> [CheckedExpr] -> CheckedExpr
    mkTensorSeq ds = SeqExpr ann tElem (mkTensor ann tElem ds)

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
nfQuantifierIn :: MonadNorm e m
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
  let unitExpr = BoolLiteralExpr ann (argExpr tRes) unit
  let tResCont = substContainerType tRes (argExpr tCont)
  let mappedContainer = MapExpr  ann (argExpr tElem) (argExpr tRes) [lam, container]
  let foldedContainer = FoldExpr ann (argExpr tRes) tResCont (argExpr tRes) (fmap (ExplicitArg ann) [bopExpr, unitExpr, mappedContainer])
  Just (nf foldedContainer)

substContainerType :: CheckedArg -> CheckedExpr -> CheckedExpr
substContainerType newTElem (App ann1 (BuiltinContainerType ann2 List)   [_tElem]) =
  App ann1 (BuiltinContainerType ann2 List) [newTElem]
substContainerType newTElem (App ann1 (BuiltinContainerType ann2 Tensor) [_tElem, tDims])  =
  App ann1 (BuiltinContainerType ann2 Tensor) [newTElem, tDims]
substContainerType _ _ = developerError "Provided an invalid container type"

quantImplementation :: Quantifier -> (BooleanOp2, Bool)
quantImplementation All = (And, True)
quantImplementation Any = (Or,  False)

--------------------------------------------------------------------------------
-- Normalising not

-- TODO implement idempotence rules?
nfNot :: MonadNorm e m
      => CheckedAnn
      -> CheckedExpr
      -> CheckedExpr
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfNot ann t _tc arg = case argExpr arg of
  -- Literal normalisation
  BoolLiteralExpr _ann tBool b -> Just $ return $ BoolLiteralExpr ann tBool (not b)

  -- Negation juggling
  (QuantifierExpr q   _ann binder body) -> Just $ QuantifierExpr (neg q) ann binder <$> nf (NotExpr ann t [ExplicitArg ann body])
  (OrderExpr      o   _ann t1 t2 args)  -> Just $ return $ OrderExpr    (neg o)     ann t1 t2 args
  (EqualityExpr   eq  _ann t1 t2 args)  -> Just $ return $ EqualityExpr (neg eq) ann t1 t2 args
  (BooleanOp2Expr op2 _ann _t [e1,e2])  -> case op2 of
    Impl -> Just $ nf $ BooleanOp2Expr And ann t [e1, negateExpr e2]
    Or   -> Just $ nf $ BooleanOp2Expr And ann t [negateExpr e1, negateExpr e2]
    And  -> Nothing -- Just $ nf $ mkOr'  ann t tc (neg $ argExpr e1) (neg $ argExpr e2)
  _                                -> Nothing
  where
    negateExpr :: CheckedArg -> CheckedArg
    negateExpr a = ExplicitArg ann (NotExpr ann t [a])

-----------------------------------------------------------------------------
-- Normalising negation

nfNeg :: MonadNorm e m
      => CheckedAnn
      -> CheckedArg
      -> CheckedArg
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfNeg ann t _tc arg = case argHead arg of
  (LitInt _ x) -> Just $ return $ IntLiteralExpr ann (argExpr t) (- x)
  (LitRat _ x) -> Just $ return $ RatLiteralExpr ann (argExpr t) (- x)
  _            -> Nothing

-----------------------------------------------------------------------------
-- Normalising map

nfMap :: MonadNorm e m
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
    let tCont = ListType ann tElem
    return $ SeqExpr ann tElem tCont ys
  _        -> Nothing
