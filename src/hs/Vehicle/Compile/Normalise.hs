module Vehicle.Compile.Normalise
  ( NormalisationOptions(..)
  , defaultNormalisationOptions
  , normalise
  ) where

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.State (MonadState(..), evalStateT, gets, modify)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.List.Split (chunksOf)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)

import Vehicle.Language.Print (prettySimple)
import Vehicle.Compile.Prelude hiding (DeclCtx)
import Vehicle.Compile.AlphaEquivalence ( alphaEq )

-- |Run a function in 'MonadNorm'.
normalise :: (MonadLogger m, Norm a) => NormalisationOptions -> a -> m a
normalise options x = do
  logDebug "Beginning normalisation"
  result <- evalStateT (runReaderT (nf x) options) mempty
  logDebug "Finished normalisation\n"
  return result

--------------------------------------------------------------------------------
-- Setup

type DeclCtx = M.Map Identifier CheckedExpr

data NormalisationOptions = Options
  { implicationsToDisjunctions :: Bool
  , subtractionToAddition      :: Bool
  , expandOutPolynomials       :: Bool
  }

defaultNormalisationOptions :: NormalisationOptions
defaultNormalisationOptions = Options
  { implicationsToDisjunctions = False
  , subtractionToAddition      = False
  , expandOutPolynomials       = False
  }

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

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m =
  ( MonadLogger m
  , MonadState DeclCtx m
  , MonadReader NormalisationOptions m
  )

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

      PrimDict ann tc     -> PrimDict ann <$> nf tc
      LSeq ann dict exprs -> LSeq ann dict <$> traverse nf exprs
      Lam ann binder expr -> Lam ann <$> nf binder <*> nf expr
      Pi ann binder body  -> Pi ann <$> nf binder <*> nf body

      Ann _ann expr _typ  -> nf expr

      Var _ (Bound _)     -> return e
      Var _ (Free ident)  -> gets (fromMaybe e . M.lookup ident)

      Let _ann letValue _binder letBody -> do
        letValue' <- nf letValue
        letBody'  <- nf letBody
        let letBodyWithSubstitution = substInto letValue' letBody'
        nf letBodyWithSubstitution

      App ann fun args -> do
        nFun  <- nf fun
        -- TODO temporary hack please remove in future once we actually have computational
        -- behaviour in implicit/instance arguments
        nArgs <- traverse (traverseExplicitArgExpr nf) args
        nfApp ann nFun nArgs

instance Norm CheckedBinder where
  nf = traverseBinderType nf

instance Norm CheckedArg where
  nf (ExplicitArg ann e) = ExplicitArg ann <$> nf e
  nf arg@Arg{}           = return arg

--------------------------------------------------------------------------------
-- Application

nfApp :: MonadNorm m => CheckedAnn -> CheckedExpr -> NonEmpty CheckedArg -> m CheckedExpr
nfApp ann fun@Lam{} args = nfAppLam ann fun (NonEmpty.toList args)
nfApp ann fun       args = let e = App ann fun args in do
  Options{..} <- ask
  fromMaybe (return e) $ case e of
    -- Binary relations
    (EqualityExpr eq _ tElem tRes [arg1, arg2]) -> nfEq eq ann tElem tRes arg1 arg2
    (OrderExpr order _ tElem tRes [arg1, arg2]) -> nfOrder order ann tElem tRes arg1 arg2

    -- Boolean operations
    (NotExpr  _ t [arg])             -> nfNot     ann t arg
    (AndExpr  _ t [arg1, arg2])      -> nfAnd     ann t arg1 arg2
    (OrExpr   _ t [arg1, arg2])      -> nfOr      ann t arg1 arg2
    (ImplExpr _ t [arg1, arg2])      -> nfImplies ann t arg1 arg2 implicationsToDisjunctions
    (IfExpr _ _ [cond, e1, e2])      -> nfIf cond e1 e2
    (QuantifierExpr q _ binder body) -> nfQuantifier ann q binder body

    -- Binary numeric ops
    (AddExpr _ t _  [arg1, arg2]) -> nfAdd ann t arg1 arg2
    (SubExpr _ t tc [arg1, arg2]) -> nfSub ann t tc arg1 arg2 subtractionToAddition
    (MulExpr _ t tc [arg1, arg2]) -> nfMul ann t tc arg1 arg2 expandOutPolynomials
    (DivExpr _ t _  [arg1, arg2]) -> nfDiv ann t arg1 arg2
    (NegExpr _ t    [arg])        -> nfNeg ann t arg expandOutPolynomials

    -- Containers
    (ConsExpr _ _ [x, xs])              -> nfCons ann x xs
    (MapExpr _ tElem tRes [fn, cont])   -> nfMap  ann tElem tRes (argExpr fn) (argExpr cont)
    (AtExpr _ _ _ _ [tensor, index])    -> nfAt   tensor index
    (FoldExpr _ _ _ _ [op, unit, cont]) -> nfFold ann op unit cont
    (QuantifierInExpr q _ tCont tRes binder body container) ->
      nfQuantifierIn ann q tCont tRes binder body container

    -- Fall-through case
    _ -> Nothing

nfAppLam :: MonadNorm m => CheckedAnn -> CheckedExpr -> [CheckedArg] -> m CheckedExpr
nfAppLam ann (Lam _ _ body) (arg : args) = nfAppLam ann (substInto (argExpr arg) body) args
nfAppLam ann fun              args       = nf (normAppList ann fun args)

--------------------------------------------------------------------------------
-- Normalising equality

nfEq :: MonadNorm m
     => Equality
     -> CheckedAnn
     -> CheckedExpr
     -> BooleanType
     -> CheckedArg
     -> CheckedArg
     -> Maybe (m CheckedExpr)
nfEq eq ann _tElem tRes e1 e2 = case (argExpr e1, argExpr e2) of
  -- Simple literal comparisons
  (BoolLiteralExpr _ _ b, BoolLiteralExpr _ _ c) -> Just $ return $ BoolLiteralExpr ann tRes (eqOp eq b c)
  (NatLiteralExpr  _ _ m, NatLiteralExpr  _ _ n) -> Just $ return $ BoolLiteralExpr ann tRes (eqOp eq m n)
  (IntLiteralExpr  _ _ i, IntLiteralExpr  _ _ j) -> Just $ return $ BoolLiteralExpr ann tRes (eqOp eq i j)
  (RatLiteralExpr  _ _ p, RatLiteralExpr  _ _ q) -> Just $ return $ BoolLiteralExpr ann tRes (eqOp eq p q)
  -- Alpha equality
  (e1', e2') | alphaEq e1' e2' -> Just $ return $ BoolLiteralExpr ann tRes (eq == Eq)
  -- If a sequence then normalise to equality over elements
  (SeqExpr _ tSeqElem tCont xs, SeqExpr _ _ _ ys)
    | length xs /= length ys -> Just $ return $ BoolLiteralExpr ann tRes False
    | otherwise ->
      let equalities    = zipWith (\x y -> EqualityExpr eq ann tSeqElem tRes (explicitArgs [x,y])) xs ys in
      let tBool         = BuiltinBooleanType ann tRes in
      let tBoolCont     = substContainerType tBool tCont in
      let equalitiesSeq = SeqExpr ann tBool tBoolCont equalities in
      Just $ nf $ booleanBigOp (logicOp eq) ann tRes tBoolCont equalitiesSeq
  -- Otherwise no normalisation
  _ -> Nothing
  where
    eqOp :: Eq a => Equality -> (a -> a -> Bool)
    eqOp Eq  = (==)
    eqOp Neq = (/=)

    logicOp :: Equality -> BooleanOp2
    logicOp Eq  = And
    logicOp Neq = Or

    explicitArgs :: [CheckedExpr] -> [CheckedArg]
    explicitArgs = fmap (ExplicitArg ann)

--------------------------------------------------------------------------------
-- Normalising orders

nfOrder :: MonadNorm m
        => Order
        -> CheckedAnn
        -> CheckedExpr
        -> BooleanType
        -> CheckedArg
        -> CheckedArg
        -> Maybe (m CheckedExpr)
nfOrder order ann _tElem tRes arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (NatLiteralExpr _ _ m, NatLiteralExpr _ _ n) -> Just $ return $ BoolLiteralExpr ann tRes (op order m n)
  (IntLiteralExpr _ _ i, IntLiteralExpr _ _ j) -> Just $ return $ BoolLiteralExpr ann tRes (op order i j)
  (RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> Just $ return $ BoolLiteralExpr ann tRes (op order x y)
  (e1 , e2) | alphaEq e1 e2                    -> Just $ return $ BoolLiteralExpr ann tRes (not (isStrict order))
  _                                            -> Nothing
  where
    op :: Ord a => Order -> (a -> a -> Bool)
    op Le = (<=)
    op Lt = (<)
    op Ge = (>=)
    op Gt = (>)

--------------------------------------------------------------------------------
-- Normalising quantification over types

nfQuantifier :: MonadNorm m
             => CheckedAnn
             -> Quantifier
             -> CheckedBinder
             -> CheckedExpr
             -> Maybe (m CheckedExpr)
nfQuantifier ann q binder body = case typeOf binder of
  -- If we're quantifing over a tensor, instead quantify over each individual
  -- element,  and then substitute in a LSeq construct with those elements in.
  (TensorType _ tElem tDims) ->
    case getDimensions (exprHead tDims) of
      Nothing -> Nothing
      Just dims -> Just $ do
        -- Calculate the dimensions of the tensor
        let tensorSize = product dims

        -- Use the list monad to create a nested list of all possible indices into the tensor
        let allIndices = traverse (\dim -> [0..dim-1]) dims
        -- Generate the corresponding names from the indices
        let allNames   = map (mkNameWithIndices (getBinderSymbol binder)) (reverse allIndices)

        -- Generate a list of variables, one for each index
        let allExprs   = map (\i -> Var ann (Bound i)) (reverse [0..tensorSize-1])
        -- Construct the corresponding nested tensor expression
        let tensor     = makeTensorLit ann tElem dims allExprs
        -- We're introducing `tensorSize` new binder so lift the indices in the body accordingly
        let body1      = liftFreeDBIndices tensorSize body
        -- Substitute throught the tensor expression for the old top-level binder
        body2 <- nf $ substIntoAtLevel tensorSize tensor body1

        -- Generate a expression prepended with `tensorSize` quantifiers
        return $ mkQuantifierSeq q ann (map Just allNames) tElem body2

  -- If we're quantifying over a finite index type then expand out to a list
  -- of indices up to the max value.
  t@(FinType _ size) ->
    case getDimension size of
      Nothing -> Nothing
      Just n  -> do
        let tCont = ListType ann t
        let cont  = SeqExpr ann t tCont (fmap (NatLiteralExpr ann t) [0 .. n-1])
        let e = QuantifierInExpr q ann tCont Prop binder body cont
        Just $ nf e

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
getDimensions (LSeq _ _ xs) = traverse getDimension xs
getDimensions _             = Nothing

getDimension :: CheckedExpr -> Maybe Int
getDimension e = case exprHead e of
  (Literal _ (LNat i)) -> Just i
  _                    -> Nothing

--------------------------------------------------------------------------------
-- Normalising quantification over lists

-- |Elaborate quantification over the members of a container type.
-- Expands e.g. `forall x in list . y` to `fold and true (map (\x -> y) list)`
nfQuantifierIn :: MonadNorm m
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
-- Normalising boolean operations

notArg :: MonadNorm m => BooleanType -> CheckedArg -> m CheckedArg
notArg t x = let ann = annotationOf x in ExplicitArg ann <$> nf (NotExpr ann t [x])

nfNot :: forall m . MonadNorm m
      => CheckedAnn
      -> BooleanType
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfNot ann t arg = case argExpr arg of
  BoolLiteralExpr    _ tBool b       -> Just $ return $ BoolLiteralExpr ann tBool (not b)
  OrderExpr      o   _ tElem _t args -> Just $ return $ OrderExpr      (neg o)  ann tElem t args
  EqualityExpr   eq  _ tElem _t args -> Just $ return $ EqualityExpr   (neg eq) ann tElem t args
  QuantifierExpr q   _ binder body   -> Just $ do
    let nBody = NotExpr ann t [ExplicitArg ann body]
    QuantifierExpr (neg q) ann binder <$> nf nBody
  ImplExpr           _ _t [e1, e2] -> Just $ do
    ne2 <- notArg t e2;
    return $ AndExpr ann t [e1, ne2]
  OrExpr             _ _t [e1, e2] -> Just $ do
    ne1 <- notArg t e1;
    ne2 <- notArg t e2;
    return $ AndExpr ann t [ne1, ne2]
  AndExpr            _ _t [e1, e2] -> Just $ do
    ne1 <- notArg t e1;
    ne2 <- notArg t e2;
    return $ OrExpr  ann t [ne1, ne2]
  _ -> Nothing

nfAnd :: forall m . MonadNorm m
      => CheckedAnn
      -> BooleanType
      -> CheckedArg
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfAnd ann t arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (BoolLiteralExpr _ _ True,  _) -> Just $ return $ argExpr arg2
  (BoolLiteralExpr _ _ False, _) -> Just $ return $ BoolLiteralExpr ann t False
  (_, BoolLiteralExpr _ _ True)  -> Just $ return $ argExpr arg1
  (_, BoolLiteralExpr _ _ False) -> Just $ return $ BoolLiteralExpr ann t False
  (e1, e2) | alphaEq e1 e2       -> Just $ return e1
  _                              -> Nothing

nfOr :: forall m . MonadNorm m
     => CheckedAnn
     -> BooleanType
     -> CheckedArg
     -> CheckedArg
     -> Maybe (m CheckedExpr)
nfOr ann t arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (BoolLiteralExpr _ _ True,  _) -> Just $ return $ BoolLiteralExpr ann t True
  (BoolLiteralExpr _ _ False, _) -> Just $ return $ argExpr arg2
  (_, BoolLiteralExpr _ _ True)  -> Just $ return $ BoolLiteralExpr ann t True
  (_, BoolLiteralExpr _ _ False) -> Just $ return $ argExpr arg1
  (e1, e2) | alphaEq e1 e2       -> Just $ return e1
  -- TODO implement zero/identity/associativity rules?
  _                              -> Nothing

nfImplies :: forall m . MonadNorm m
          => CheckedAnn
          -> BooleanType
          -> CheckedArg
          -> CheckedArg
          -> Bool
          -> Maybe (m CheckedExpr)
nfImplies ann t arg1 arg2 convertToOr = case (argExpr arg1, argExpr arg2) of
  (BoolLiteralExpr _ _ True,  _) -> Just $ return $ argExpr arg2
  (BoolLiteralExpr _ _ False, _) -> Just $ return $ BoolLiteralExpr ann t True
  (_, BoolLiteralExpr _ _ True)  -> Just $ return $ BoolLiteralExpr ann t True
  (_, BoolLiteralExpr _ _ False) -> Just $ return $ NotExpr ann t [arg2]
  (e1, e2) | alphaEq e1 e2       -> Just $ return $ BoolLiteralExpr ann t True
  _        | convertToOr         -> Just $ do
    negArg1 <- notArg t arg1
    return $ OrExpr ann t [negArg1, arg2]
  _                              -> Nothing

nfIf :: forall m . MonadNorm m
     => CheckedArg
     -> CheckedArg
     -> CheckedArg
     -> Maybe (m CheckedExpr)
nfIf condition e1 e2 = case argExpr condition of
  BoolLiteralExpr _ _ True  -> Just $ return $ argExpr e1
  BoolLiteralExpr _ _ False -> Just $ return $ argExpr e2
  _                         -> Nothing

-----------------------------------------------------------------------------
-- Normalising negation

negArg :: MonadNorm m => NumericType -> CheckedArg -> m CheckedArg
negArg t x = let ann = annotationOf x in ExplicitArg ann <$> nf (NegExpr ann t [x])

nfAdd :: MonadNorm m
      => CheckedAnn
      -> NumericType
      -> CheckedArg
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfAdd ann t arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  -- TODO implement zero/identity/associativity rules?
  (NatLiteralExpr _ _ m, NatLiteralExpr _ _ n) -> Just $ return $ NatLiteralExpr ann (BuiltinNumericType ann t) (m + n)
  (IntLiteralExpr _ _ i, IntLiteralExpr _ _ j) -> Just $ return $ IntLiteralExpr ann t (i + j)
  (RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> Just $ return $ RatLiteralExpr ann t (x + y)
  _                                            -> Nothing

nfSub :: MonadNorm m
      => CheckedAnn
      -> NumericType
      -> CheckedExpr
      -> CheckedArg
      -> CheckedArg
      -> Bool
      -> Maybe (m CheckedExpr)
nfSub ann t tc arg1 arg2 convertToAddition = case (argExpr arg1, argExpr arg2) of
  -- TODO implement zero/identity/associativity rules?
  (IntLiteralExpr _ _ i, IntLiteralExpr _ _ j) -> Just $ return $ IntLiteralExpr ann t (i - j)
  (RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> Just $ return $ RatLiteralExpr ann t (x - y)
  (_, _) | convertToAddition                   -> Just $ do
    negArg2 <- negArg t arg2
    return $ AddExpr ann t tc [arg1, negArg2]
  _ -> Nothing

nfMul :: MonadNorm m
      => CheckedAnn
      -> NumericType
      -> CheckedExpr
      -> CheckedArg
      -> CheckedArg
      -> Bool
      -> Maybe (m CheckedExpr)
nfMul ann t tc arg1 arg2 expandOut = case (argExpr arg1, argExpr arg2) of
  -- TODO implement zero/identity/associativity rules?
  (IntLiteralExpr _ _ i, IntLiteralExpr _ _ j) -> Just $ return $ IntLiteralExpr ann t (i * j)
  (RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> Just $ return $ RatLiteralExpr ann t (x * y)
  -- Expanding out multiplication
  (_, NumericOp2Expr op _ _ _ [v1, v2])
    | expandOut && (op == Add || op == Sub) -> Just $ nf $ distribute op arg1 v1 arg1 v2
  (NumericOp2Expr op _ _ _ [v1, v2], _)
    | expandOut && (op == Add || op == Sub) -> Just $ nf $ distribute op v1 arg2 v2 arg2
  _    -> Nothing
  where
    distribute :: NumericOp2 -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr
    distribute op x1 y1 x2 y2 = NumericOp2Expr op ann t tc $ fmap (ExplicitArg ann)
        [ MulExpr ann t tc [x1, y1]
        , MulExpr ann t tc [x2, y2]
        ]

nfDiv :: MonadNorm m
      => CheckedAnn
      -> NumericType
      -> CheckedArg
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfDiv ann t arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  -- TODO implement zero/identity/associativity rules?
  (RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> Just $ return $ RatLiteralExpr ann t (x / y)
  _                                            -> Nothing

nfNeg :: MonadNorm m
      => CheckedAnn
      -> NumericType
      -> CheckedArg
      -> Bool
      -> Maybe (m CheckedExpr)
nfNeg _ann t e expandOut = case argExpr e of
  NatLiteralExpr ann _ x                -> Just $ return $ IntLiteralExpr ann t (- x)
  IntLiteralExpr ann _ x                -> Just $ return $ IntLiteralExpr ann t (- x)
  RatLiteralExpr ann _ x                -> Just $ return $ RatLiteralExpr ann t (- x)
  NegExpr _ _ [e']                      -> Just $ return $ argExpr e'
  AddExpr ann _ tc [e1, e2] | expandOut -> Just $ do
    ne1 <- negArg t e1
    ne2 <- negArg t e2
    nf $ AddExpr ann t tc [ne1, ne2]
  MulExpr ann _ tc [e1, e2] | expandOut -> Just $ do
    ne1 <- negArg t e1
    nf $ AddExpr ann t tc [ne1, e2]
  _                           -> Nothing

-----------------------------------------------------------------------------
-- Normalising container operations

nfCons :: MonadNorm m
       => CheckedAnn
       -> CheckedArg
       -> CheckedArg
       -> Maybe (m CheckedExpr)
nfCons ann x xs = case argExpr xs of
  SeqExpr _ tElem tList es -> Just $ return $ SeqExpr ann tElem tList (argExpr x : es)
  _                        -> Nothing

nfMap :: MonadNorm m
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

nfAt :: MonadNorm m
     => CheckedArg
     -> CheckedArg
     -> Maybe (m CheckedExpr)
nfAt tensor index = case (argExpr tensor, argExpr index) of
  (SeqExpr _ _ _ es, NatLiteralExpr _ _ i) -> Just $ return $ es !! fromIntegral i
  _                                        -> Nothing

nfFold :: MonadNorm m
       => CheckedAnn
       -> CheckedArg
       -> CheckedArg
       -> CheckedArg
       -> Maybe (m CheckedExpr)
nfFold ann foldOp unit container = case argExpr container of
  SeqExpr _ _ _ xs -> do
    let combine x body = normApp ann (argExpr foldOp) [ExplicitArg ann x, ExplicitArg ann body]
    Just $ nf $ foldr combine (argExpr unit) xs
  _ -> Nothing
  -- TODO distribute over cons