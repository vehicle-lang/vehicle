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
import Vehicle.Compile.AlphaEquivalence ( alphaEq )
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

      PrimDict ann tc     -> PrimDict ann <$> nf tc
      Seq ann exprs       -> Seq ann <$> traverse nf exprs
      Lam ann binder expr -> Lam ann <$> nf binder <*> nf expr
      Pi ann binder body  -> Pi ann <$> nf binder <*> nf body

      Ann _ann expr _typ  -> nf expr

      Var _ (Bound _)     -> return e
      Var _ (Free ident)  -> gets (fromMaybe e . M.lookup ident)

      Let ann letValue binder letBody -> do
        letValue' <- nf letValue
        binder'   <- nf binder
        letBody'  <- nf letBody
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
nfApp ann  fun@(Builtin _ _) args      = do
  let e = normAppList ann fun args
  case e of
    -- Equality
    (EqualityExpr eq _ann _tElem tRes [arg1, arg2]) ->
      fromMaybe (return e) (nfEq eq ann tRes (argExpr arg1) (argExpr arg2))

    -- Not
    (NotExpr _ tRes [arg]) ->
      fromMaybe (return e) (nfNot ann tRes arg)

    -- Binary boolean operations
    (BooleanOp2Expr op2 _ t [arg1, arg2]) -> case (op2, argExpr arg1, argExpr arg2) of
      -- TODO implement associativity rules?
      -- See https://github.com/wenkokke/vehicle/issues/2
      (And, BoolLiteralExpr _ _ True,  _) -> return $ argExpr arg2
      (And, BoolLiteralExpr _ _ False, _) -> return $ BoolLiteralExpr ann t False
      (And, _, BoolLiteralExpr _ _ True)  -> return $ argExpr arg1
      (And, _, BoolLiteralExpr _ _ False) -> return $ BoolLiteralExpr ann t False

      (Or, BoolLiteralExpr _ _ True,  _) -> return $ BoolLiteralExpr ann t True
      (Or, BoolLiteralExpr _ _ False, _) -> return $ argExpr arg2
      (Or, _, BoolLiteralExpr _ _ True)  -> return $ BoolLiteralExpr ann t True
      (Or, _, BoolLiteralExpr _ _ False) -> return $ argExpr arg1

      (Impl, BoolLiteralExpr _ _ True,  _) -> return $ argExpr arg2
      (Impl, BoolLiteralExpr _ _ False, _) -> return $ BoolLiteralExpr ann t True
      (Impl, _, BoolLiteralExpr _ _ True)  -> return $ BoolLiteralExpr ann t True
      (Impl, _, BoolLiteralExpr _ _ False) -> return $ NotExpr ann t [arg2]

      _ -> return e

    -- If
    (IfExpr _ _tRes cond e1 e2) -> case cond of
      BoolLiteralExpr _ _ True  -> return e1
      BoolLiteralExpr _ _ False -> return e2
      _                         -> return e

    -- Le
    (OrderExpr Le _ _t1 tRes [arg1, arg2]) -> case (argExpr arg1, argExpr arg2) of
      (NatLiteralExpr _ _ m, NatLiteralExpr _ _ n) -> return $ BoolLiteralExpr ann tRes (m <= n)
      (IntLiteralExpr _ _ i, IntLiteralExpr _ _ j) -> return $ BoolLiteralExpr ann tRes (i <= j)
      (RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> return $ BoolLiteralExpr ann tRes (x <= y)
      _                                            -> return e

    -- Lt
    (OrderExpr Lt _ _t1 tRes [arg1, arg2]) -> case (argExpr arg1, argExpr arg2) of
      (NatLiteralExpr _ _ m, NatLiteralExpr _ _ n) -> return $ BoolLiteralExpr ann tRes (m < n)
      (IntLiteralExpr _ _ i, IntLiteralExpr _ _ j) -> return $ BoolLiteralExpr ann tRes (i < j)
      (RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> return $ BoolLiteralExpr ann tRes (x < y)
      _                                            -> return e

    -- Binary numeric ops
    (NumericOp2Expr op2 _ t _ [arg1, arg2]) -> case (op2, argExpr arg1, argExpr arg2) of
      -- TODO implement zero/identity/associativity rules?
      (Add, NatLiteralExpr _ _ m, NatLiteralExpr _ _ n) -> return $ NatLiteralExpr ann t (m + n)
      (Add, IntLiteralExpr _ _ i, IntLiteralExpr _ _ j) -> return $ IntLiteralExpr ann t (i + j)
      (Add, RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> return $ RatLiteralExpr ann t (x + y)

      (Sub, IntLiteralExpr _ _ i, IntLiteralExpr _ _ j) -> return $ IntLiteralExpr ann t (i - j)
      (Sub, RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> return $ RatLiteralExpr ann t (x - y)

      (Mul, IntLiteralExpr _ _ i, IntLiteralExpr _ _ j) -> return $ IntLiteralExpr ann t (i * j)
      (Mul, RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> return $ RatLiteralExpr ann t (x * y)

      (Div, RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> return $ RatLiteralExpr ann t (x / y)

      _                                                 -> return e

    -- Negation
    (NegExpr _ t [arg]) -> fromMaybe (return e) (nfNeg ann t (argExpr arg))

    -- Cons
    (ConsExpr _ tElem [x, cont]) -> case argExpr cont of
      SeqExpr _ _ _ xs -> return $ SeqExpr ann tElem (ListType ann tElem) (argExpr x : xs)
      _                -> return e

    -- Lookup
    (AtExpr _ _tElem _tDims [cont, index]) ->
      case (argExpr cont, argExpr index) of
        (SeqExpr _ _ _ es, NatLiteralExpr _ _ i) -> return $ es !! fromIntegral i
        _                                        -> return e

    -- Map
    (MapExpr _ tElem tRes [fn, cont]) ->
      fromMaybe (return e) (nfMap ann tElem tRes (argExpr fn) (argExpr cont))

    -- TODO distribute over cons

    -- Fold
    (FoldExpr _ _tElem _tCont _tRes [foldOp, unit, cont]) -> case argExpr cont of
      SeqExpr _ _ _ xs -> nf $ foldr (\x body -> normApp ann (argExpr foldOp) [ExplicitArg ann x, ExplicitArg ann body]) (argExpr unit) xs
      _                -> return e
    -- TODO distribute over cons

    -- Quantifiers
    (QuantifierExpr q _ binder body) ->
      fromMaybe (return e) (nfQuantifier ann q binder body)

    -- Quantifiers over containers
    (QuantifierInExpr q _ tCont tRes binder body container) ->
      fromMaybe (return e) (nfQuantifierIn ann q tCont tRes binder body container)

    -- Fall-through case
    _ -> return e

nfApp ann fun args = return $ normAppList ann fun args

--------------------------------------------------------------------------------
-- Normalising equality

nfEq :: MonadNorm e m
     => Equality
     -> CheckedAnn
     -> BooleanType
     -> CheckedExpr
     -> CheckedExpr
     -> Maybe (m CheckedExpr)
nfEq eq ann tRes e1 e2 = if alphaEq e1 e2
  then Just $ return $ BoolLiteralExpr ann tRes (eq == Eq)
  else case (e1, e2) of
    (NatLiteralExpr _ _ m, NatLiteralExpr _ _ n) -> Just $ return $ BoolLiteralExpr ann tRes (eqOp eq m n)
    (IntLiteralExpr _ _ i, IntLiteralExpr _ _ j) -> Just $ return $ BoolLiteralExpr ann tRes (eqOp eq i j)
    (RatLiteralExpr _ _ p, RatLiteralExpr _ _ q) -> Just $ return $ BoolLiteralExpr ann tRes (eqOp eq p q)
    (SeqExpr _ tElem tCont xs, SeqExpr _ _ _ ys) -> Just $
      if length xs /= length ys then
        return $ BoolLiteralExpr ann tRes False
      else
        let equalities    = zipWith (\x y -> EqualityExpr eq ann tElem tRes (map (ExplicitArg ann) [x,y])) xs ys in
        let tBool         = BuiltinBooleanType ann tRes in
        let tBoolCont     = substContainerType tBool tCont in
        let equalitiesSeq = SeqExpr ann tBool tBoolCont equalities in
        nf $ booleanBigOp (logicOp eq) ann tRes tBoolCont equalitiesSeq
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
             -> CheckedBinder
             -> CheckedExpr
             -> Maybe (m CheckedExpr)
nfQuantifier ann q binder body = case typeOf binder of
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

makeTensorLit :: CheckedAnn -> CheckedExpr -> [Int] -> [CheckedExpr] -> CheckedExpr
makeTensorLit ann tElem dims exprs = assert (product dims == length exprs) (go dims exprs)
  where
    mkTensorSeq :: [Int] -> [CheckedExpr] -> CheckedExpr
    mkTensorSeq ds = SeqExpr ann tElem (mkTensorType ann tElem ds)

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
               -> CheckedExpr  -- tCont
               -> BooleanType -- tRes
               -> CheckedBinder
               -> CheckedExpr
               -> CheckedExpr
               -> Maybe (m CheckedExpr)
nfQuantifierIn ann q tCont boolType binder body container = do
  let tRes = BuiltinBooleanType ann boolType
  let tResCont = substContainerType tRes tCont
  let mappedContainer = MapExpr ann (typeOf binder) tRes (ExplicitArg ann <$> [Lam ann binder body, container])
  let foldedContainer = booleanBigOp (quantOp q) ann boolType tResCont mappedContainer
  Just (nf foldedContainer)

substContainerType :: CheckedExpr -> CheckedExpr -> CheckedExpr
substContainerType newTElem (ListType   ann _tElem)       = ListType   ann newTElem
substContainerType newTElem (TensorType ann _tElem tDims) = TensorType ann newTElem tDims
substContainerType _ _ = developerError "Provided an invalid container type"

quantOp :: Quantifier -> BooleanOp2
quantOp All = And
quantOp Any = Or

--------------------------------------------------------------------------------
-- Normalising not

-- TODO implement idempotence rules?
nfNot :: MonadNorm e m
      => CheckedAnn
      -> BooleanType
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfNot ann t arg = case argExpr arg of
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
      -> NumericType
      -> CheckedExpr
      -> Maybe (m CheckedExpr)
nfNeg ann t e = case e of
  (IntLiteralExpr _ _ x) -> Just $ return $ IntLiteralExpr ann t (- x)
  (RatLiteralExpr _ _ x) -> Just $ return $ RatLiteralExpr ann t (- x)
  _                      -> Nothing

-----------------------------------------------------------------------------
-- Normalising map

nfMap :: MonadNorm e m
      => CheckedAnn
      -> CheckedExpr
      -> CheckedExpr
      -> CheckedExpr
      -> CheckedExpr
      -> Maybe (m CheckedExpr)
nfMap ann _tFrom tTo fun container = case container of
  SeqExpr _ _ _ xs -> Just $ do
    ys <- traverse (\x -> nf $ normApp ann fun [ExplicitArg ann x]) xs
    let tCont = ListType ann tTo
    return $ SeqExpr ann tTo tCont ys
  _        -> Nothing