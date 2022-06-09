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

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile.AlphaEquivalence ( alphaEq )
import Vehicle.Compile.Error

-- |Run a function in 'MonadNorm'.
normalise :: (MonadCompile m, Norm a, PrettyWith ('Named ('As 'External)) ([DBBinding], a))
          => a
          -> NormalisationOptions
          -> m a
normalise x options@Options{..} = logCompilerPass currentPass $ do
  result <- evalStateT (runReaderT (nf x) options) declContext
  logCompilerPassOutput (prettyFriendlyDB boundContext result)
  return result

currentPass :: Doc ()
currentPass = "normalisation"

--------------------------------------------------------------------------------
-- Setup

data NormalisationOptions = Options
  { implicationsToDisjunctions :: Bool
  , subtractionToAddition      :: Bool
  , expandOutPolynomials       :: Bool
  , declContext                :: DeclCtx
  , boundContext               :: [DBBinding]
  }

defaultNormalisationOptions :: NormalisationOptions
defaultNormalisationOptions = Options
  { implicationsToDisjunctions = False
  , subtractionToAddition      = False
  , expandOutPolynomials       = False
  , declContext                = mempty
  , boundContext               = mempty
  }

--------------------------------------------------------------------------------
-- Debug functions

showEntry :: MonadNorm m => CheckedExpr -> m CheckedExpr
showEntry e = do
  logDebug MaxDetail ("norm-entry " <> prettySimple e)
  incrCallDepth
  return e

showExit :: MonadNorm m => CheckedExpr -> m CheckedExpr -> m CheckedExpr
showExit old mNew = do
  new <- mNew
  decrCallDepth
  when (old /= new) $
    logDebug MaxDetail ("normalising" <+> prettySimple old)
  logDebug MaxDetail ("norm-exit " <+> prettySimple new)
  return new

--------------------------------------------------------------------------------
-- Normalisation algorithm

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m =
  ( MonadCompile m
  , MonadState DeclCtx m
  , MonadReader NormalisationOptions m
  )

-- |Class for the various normalisation functions.
-- Invariant is that everything in the context is fully normalised
class Norm vf where
  nf :: MonadNorm m => vf -> m vf

instance Norm CheckedProg where
  nf (Main decls) = Main <$> traverse nf decls

instance Norm CheckedDecl where
  nf = \case
    DefResource ann r ident typ ->
      DefResource ann r ident <$> nf typ

    DefFunction ann u ident typ expr -> do
      typ'  <- nf typ
      expr' <- nf expr
      modify (M.insert ident (typ, Just expr'))
      return $ DefFunction ann u ident typ' expr'

instance Norm CheckedExpr where
  nf e = showExit e $ do
    e' <- showEntry e
    case e' of
      Type{}      -> return e
      Hole{}      -> return e
      Literal{}   -> return e
      Builtin{}   -> return e
      Meta{}      -> resolutionError currentPass "meta"

      PrimDict ann tc     -> PrimDict ann <$> nf tc
      LSeq ann dict exprs -> LSeq ann dict <$> traverse nf exprs
      Lam ann binder expr -> Lam ann <$> nf binder <*> nf expr
      Pi ann binder body  -> Pi ann <$> nf binder <*> nf body

      Ann _ann expr _typ  -> nf expr

      Var _ (Bound _)     -> return e
      Var _ (Free ident)  -> gets (maybe e (fromMaybe e . snd) . M.lookup ident)

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

nfApp :: MonadNorm m => Provenance -> CheckedExpr -> NonEmpty CheckedArg -> m CheckedExpr
nfApp ann fun@Lam{} args = nfAppLam ann fun (NonEmpty.toList args)
nfApp ann fun       args = do
  let e = App ann fun args
  Options{..} <- ask
  fromMaybe (return e) $ case e of
    -- Types
    TensorType _ tElem (NilExpr _ _) -> Just $ return tElem

    -- Binary relations
    EqualityExpr eq _ tElem [arg1, arg2] -> nfEq    eq    ann tElem arg1 arg2
    OrderExpr order _ tElem [arg1, arg2] -> nfOrder order ann tElem arg1 arg2

    -- Boolean operations
    NotExpr  _ [arg]               -> nfNot     ann arg
    AndExpr  _ [arg1, arg2]        -> nfAnd     ann arg1 arg2
    OrExpr   _ [arg1, arg2]        -> nfOr      ann arg1 arg2
    ImplExpr _ [arg1, arg2]        -> nfImplies ann arg1 arg2 implicationsToDisjunctions
    IfExpr _ _ [cond, e1, e2]      -> nfIf cond e1 e2

    -- Quantifiers
    ForallExpr  _ binder body -> nfQuantifier ann Forall binder body
    ExistsExpr  _ binder body -> nfQuantifier ann Exists binder body
    ForeachExpr _ binder body -> nfForeach ann binder body

    ForallInExpr  _ tCont binder body container -> nfQuantifierIn ann Forall tCont binder body container
    ExistsInExpr  _ tCont binder body container -> nfQuantifierIn ann Exists tCont binder body container
    ForeachInExpr _ _ _   binder body container -> nfForeachIn ann binder body container

    -- Binary numeric ops
    AddExpr _ t _  [arg1, arg2] -> nfAdd ann t arg1 arg2
    SubExpr _ t tc [arg1, arg2] -> nfSub ann t tc arg1 arg2 subtractionToAddition
    MulExpr _ t tc [arg1, arg2] -> nfMul ann t tc arg1 arg2 expandOutPolynomials
    DivExpr _ t _  [arg1, arg2] -> nfDiv ann t arg1 arg2
    NegExpr _ t    [arg]        -> nfNeg ann t arg expandOutPolynomials

    -- Containers
    ConsExpr _ _ [x, xs]              -> nfCons ann x xs
    MapExpr _ tElem tRes [fn, cont]   -> nfMap  ann tElem tRes (argExpr fn) (argExpr cont)
    AtExpr _ _ _ _ [tensor, index]    -> nfAt   tensor index
    FoldExpr _ _ _ _ [op, unit, cont] -> nfFold ann op unit cont

    -- Fall-through case
    _ -> Nothing

nfAppLam :: MonadNorm m => Provenance -> CheckedExpr -> [CheckedArg] -> m CheckedExpr
nfAppLam ann (Lam _ _ body) (arg : args) = nfAppLam ann (substInto (argExpr arg) body) args
nfAppLam ann fun              args       = nf (normAppList ann fun args)

--------------------------------------------------------------------------------
-- Normalising equality

nfEq :: MonadNorm m
     => Equality
     -> Provenance
     -> CheckedExpr
     -> CheckedArg
     -> CheckedArg
     -> Maybe (m CheckedExpr)
nfEq eq ann _tElem e1 e2 = case (argExpr e1, argExpr e2) of
  -- Simple literal comparisons
  (BoolLiteralExpr _   b, BoolLiteralExpr _   c) -> Just $ return $ BoolLiteralExpr ann (eqOp eq b c)
  (NatLiteralExpr  _ _ m, NatLiteralExpr  _ _ n) -> Just $ return $ BoolLiteralExpr ann (eqOp eq m n)
  (IntLiteralExpr  _ _ i, IntLiteralExpr  _ _ j) -> Just $ return $ BoolLiteralExpr ann (eqOp eq i j)
  (RatLiteralExpr  _ _ p, RatLiteralExpr  _ _ q) -> Just $ return $ BoolLiteralExpr ann (eqOp eq p q)
  -- Alpha equality
  (e1', e2') | alphaEq e1' e2' -> Just $ return $ BoolLiteralExpr ann (eq == Eq)
  -- If a sequence then normalise to equality over elements
  (SeqExpr _ tSeqElem tCont xs, SeqExpr _ _ _ ys)
    | length xs /= length ys -> Just $ return $ FalseExpr ann
    | otherwise ->
      let mkEquality x y = EqualityExpr eq ann tSeqElem (explicitArgs [x, y]) in
      let equalities     = zipWith mkEquality xs ys in
      let tBool          = BoolType ann in
      let tBoolCont      = substContainerType tBool tCont in
      let equalitiesSeq  = SeqExpr ann tBool tBoolCont equalities in
      Just $ nf $ mkBooleanBigOp (logicOp eq) ann tBoolCont equalitiesSeq
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
        -> Provenance
        -> CheckedExpr
        -> CheckedArg
        -> CheckedArg
        -> Maybe (m CheckedExpr)
nfOrder order ann _tElem arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (NatLiteralExpr _ _ m, NatLiteralExpr _ _ n) -> Just $ return $ BoolLiteralExpr ann (op order m n)
  (IntLiteralExpr _ _ i, IntLiteralExpr _ _ j) -> Just $ return $ BoolLiteralExpr ann (op order i j)
  (RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> Just $ return $ BoolLiteralExpr ann (op order x y)
  (e1 , e2) | alphaEq e1 e2                    -> Just $ return $ BoolLiteralExpr ann (not (isStrict order))
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
             => Provenance
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
  t@(IndexType _ size) ->
    case getDimension size of
      Nothing -> Nothing
      Just n  -> do
        let tCont = ListType ann t
        let cont  = SeqExpr ann t tCont (fmap (NatLiteralExpr ann t) [0 .. n-1])
        let e = QuantifierInExpr q ann tCont binder body cont
        Just $ nf e

  _ -> Nothing

nfForeach :: MonadNorm m
          => Provenance
          -> CheckedBinder
          -> CheckedExpr
          -> Maybe (m CheckedExpr)
nfForeach ann binder body = Just $
  case typeOf binder of
    t@(IndexType _ size) ->
      case getDimension size of
        Nothing -> compilerDeveloperError
          "Encountered unexpected non-constant Index dimension during normalisation."
        Just n  -> do
          let tCont = ListType ann t
          let cont  = SeqExpr ann t tCont (fmap (NatLiteralExpr ann t) [0 .. n-1])
          let e = ForeachInExpr ann size tCont binder body cont
          nf e
    _ -> compilerDeveloperError
      "Type-checking is supposed to ensure only using `foreach` with the `Index` type."

makeTensorLit :: Provenance -> CheckedExpr -> [Int] -> [CheckedExpr] -> CheckedExpr
makeTensorLit ann tElem dims exprs = assert (product dims == length exprs) (go dims exprs)
  where
    mkTensorSeq :: [Int] -> [CheckedExpr] -> CheckedExpr
    mkTensorSeq ds = SeqExpr ann tElem (mkTensorType ann tElem ds)

    go []       [] = mkTensorSeq []       []
    go [d]      es = mkTensorSeq [d]      es
    go (d : ds) es = mkTensorSeq (d : ds) (map (go ds) (chunksOf (product ds) es))
    go []  (_ : _) = developerError "Found inhabitants of the empty dimension! Woo!"

--------------------------------------------------------------------------------
-- Normalising quantification over lists

-- |Elaborate quantification over the members of a container type.
-- Expands e.g. `forall x in list . y` to `fold and true (map (\x -> y) list)`
nfQuantifierIn :: MonadNorm m
               => Provenance
               -> Quantifier
               -> CheckedExpr  -- tCont
               -> CheckedBinder
               -> CheckedExpr
               -> CheckedExpr
               -> Maybe (m CheckedExpr)
nfQuantifierIn ann q tCont binder body container = Just $ do
  let tRes = BoolType ann
  let tResCont = substContainerType tRes tCont
  let mappedContainer = MapExpr ann (typeOf binder) tRes (ExplicitArg ann <$> [Lam ann binder body, container])
  case q of
    Forall  -> nf $ mkBooleanBigOp And ann tResCont mappedContainer
    Exists  -> nf $ mkBooleanBigOp Or  ann tResCont mappedContainer

nfForeachIn :: MonadNorm m
            => Provenance
            -> CheckedBinder
            -> CheckedExpr
            -> CheckedExpr
            -> Maybe (m CheckedExpr)
nfForeachIn ann binder body container = Just $ do
  let tRes = BoolType ann
  let lamArg = ExplicitArg ann (Lam ann binder body)
  let containerArg = ExplicitArg ann container
  return $ MapExpr ann (typeOf binder) tRes [lamArg, containerArg]

substContainerType :: CheckedExpr -> CheckedExpr -> CheckedExpr
substContainerType newTElem (ListType   ann _tElem)       = ListType   ann newTElem
substContainerType newTElem (TensorType ann _tElem tDims) = TensorType ann newTElem tDims
substContainerType _ _ = developerError "Provided an invalid container type"

--------------------------------------------------------------------------------
-- Normalising boolean operations

notArg :: MonadNorm m => CheckedArg -> m CheckedArg
notArg x = let ann = provenanceOf x in ExplicitArg ann <$> nf (NotExpr ann [x])

nfNot :: forall m . MonadNorm m
      => Provenance
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfNot ann arg = case argExpr arg of
  BoolLiteralExpr    _ b           -> Just $ return $ BoolLiteralExpr ann (not b)
  OrderExpr      o   _ tElem args  -> Just $ return $ OrderExpr      (neg o)  ann tElem args
  EqualityExpr   eq  _ tElem args  -> Just $ return $ EqualityExpr   (neg eq) ann tElem args
  ForallExpr _ binder body -> Just $ do
    let nBody = NotExpr ann [ExplicitArg ann body]
    ExistsExpr ann binder <$> nf nBody
  ExistsExpr _ binder body -> Just $ do
    let nBody = NotExpr ann [ExplicitArg ann body]
    ForallExpr ann binder <$> nf nBody
  ImplExpr           _ [e1, e2] -> Just $ do
    ne2 <- notArg e2;
    return $ AndExpr ann [e1, ne2]
  OrExpr             _ [e1, e2] -> Just $ do
    ne1 <- notArg e1;
    ne2 <- notArg e2;
    return $ AndExpr ann [ne1, ne2]
  AndExpr            _ [e1, e2] -> Just $ do
    ne1 <- notArg e1;
    ne2 <- notArg e2;
    return $ OrExpr ann [ne1, ne2]
  IfExpr _ tRes [p, e1, e2] -> Just $ do
    ne1 <- notArg e1
    ne2 <- notArg e2
    return $ IfExpr ann tRes [p, ne1, ne2]
  _ -> Nothing

nfAnd :: forall m . MonadNorm m
      => Provenance
      -> CheckedArg
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfAnd ann arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (TrueExpr  _, _) -> Just $ return $ argExpr arg2
  (FalseExpr _, _) -> Just $ return $ FalseExpr ann
  (_, TrueExpr  _) -> Just $ return $ argExpr arg1
  (_, FalseExpr _) -> Just $ return $ FalseExpr ann
  (e1, e2) | alphaEq e1 e2       -> Just $ return e1
  _                              -> Nothing

nfOr :: forall m . MonadNorm m
     => Provenance
     -> CheckedArg
     -> CheckedArg
     -> Maybe (m CheckedExpr)
nfOr ann arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (TrueExpr  _, _) -> Just $ return $ TrueExpr ann
  (FalseExpr _, _) -> Just $ return $ argExpr arg2
  (_, TrueExpr  _) -> Just $ return $ TrueExpr ann
  (_, FalseExpr _) -> Just $ return $ argExpr arg1
  (e1, e2) | alphaEq e1 e2       -> Just $ return e1
  -- TODO implement zero/identity/associativity rules?
  _                              -> Nothing

nfImplies :: forall m . MonadNorm m
          => Provenance
          -> CheckedArg
          -> CheckedArg
          -> Bool
          -> Maybe (m CheckedExpr)
nfImplies ann arg1 arg2 convertToOr = case (argExpr arg1, argExpr arg2) of
  (TrueExpr  _, _)         -> Just $ return $ argExpr arg2
  (FalseExpr _, _)         -> Just $ return $ TrueExpr ann
  (_, TrueExpr  _)         -> Just $ return $ TrueExpr ann
  (_, FalseExpr _)         -> Just $ return $ NotExpr ann [arg2]
  (e1, e2) | alphaEq e1 e2 -> Just $ return $ TrueExpr ann
  _        | convertToOr   -> Just $ do
    negArg1 <- notArg arg1
    return $ OrExpr ann [negArg1, arg2]
  _                              -> Nothing

nfIf :: forall m . MonadNorm m
     => CheckedArg
     -> CheckedArg
     -> CheckedArg
     -> Maybe (m CheckedExpr)
nfIf condition e1 e2 = case argExpr condition of
  TrueExpr  _ -> Just $ return $ argExpr e1
  FalseExpr _ -> Just $ return $ argExpr e2
  _           -> Nothing

-----------------------------------------------------------------------------
-- Normalising negation

negArg :: MonadNorm m => CheckedExpr -> CheckedArg -> m CheckedArg
negArg t x = let ann = provenanceOf x in ExplicitArg ann <$> nf (NegExpr ann t [x])

nfAdd :: MonadNorm m
      => Provenance
      -> CheckedExpr
      -> CheckedArg
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfAdd ann t arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  -- TODO implement zero/identity/associativity rules?
  (NatLiteralExpr _ _ m, NatLiteralExpr _ _ n) -> Just $ return $ NatLiteralExpr ann t (m + n)
  (IntLiteralExpr _ _ i, IntLiteralExpr _ _ j) -> Just $ return $ IntLiteralExpr ann t (i + j)
  (RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> Just $ return $ RatLiteralExpr ann t (x + y)
  _                                            -> Nothing

nfSub :: MonadNorm m
      => Provenance
      -> CheckedExpr
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
      => Provenance
      -> CheckedExpr
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
      => Provenance
      -> CheckedExpr
      -> CheckedArg
      -> CheckedArg
      -> Maybe (m CheckedExpr)
nfDiv ann t arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  -- TODO implement zero/identity/associativity rules?
  (RatLiteralExpr _ _ x, RatLiteralExpr _ _ y) -> Just $ return $ RatLiteralExpr ann t (x / y)
  _                                            -> Nothing

nfNeg :: MonadNorm m
      => Provenance
      -> CheckedExpr
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
       => Provenance
       -> CheckedArg
       -> CheckedArg
       -> Maybe (m CheckedExpr)
nfCons ann x xs = case argExpr xs of
  SeqExpr _ tElem tList es -> Just $ return $ SeqExpr ann tElem tList (argExpr x : es)
  _                        -> Nothing

nfMap :: MonadNorm m
      => Provenance
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
       => Provenance
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