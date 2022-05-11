module Vehicle.Compile.Type.TypeClass
  ( solveTypeClassConstraint
  , solveDefaultTypeClassConstraints
  ) where

import Data.Maybe ( maybeToList )
import Data.Map (Map)
import Data.Map qualified as Map
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad (unless, forM)
import Control.Monad.Except ( throwError )

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.WeakHeadNormalForm
import Vehicle.Language.Print (prettyVerbose)

--------------------------------------------------------------------------------
-- Solution

solveTypeClassConstraint :: MonadConstraintSolving m
                         => ConstraintContext
                         -> TypeClassConstraint
                         -> m ConstraintProgress
solveTypeClassConstraint ctx (m `Has` (App ann tc@(BuiltinTypeClass{}) args)) = do
  -- We first normalise the type-class args to weak-head normal form
  eWHNF <- App ann tc <$> traverse (traverseArgExpr (whnfWithMetas (varContext ctx))) args
  let constraint = TC ctx (m `Has` eWHNF)

  -- Then check which sort of type-class it is:
  progress <- blockOnMetas eWHNF $ case eWHNF of
    HasEqExpr            _ t       -> solveHasEq          constraint t
    HasOrdExpr           _ t       -> solveHasOrd         constraint t
    HasNatOpsExpr        _ t       -> solveHasNatOps      constraint t
    HasIntOpsExpr        _ t       -> solveHasIntOps      constraint t
    HasRatOpsExpr        _ t       -> solveHasRatOps      constraint t
    HasConOpsExpr        _ t1 t2   -> solveHasConOps      constraint t1 t2
    HasNatLitsUpToExpr   _ n t     -> solveHasNatLits     constraint n t
    HasIntLitsExpr       _ t       -> solveHasIntLits     constraint t
    HasRatLitsExpr       _ t       -> solveHasRatLits     constraint t
    HasConLitsOfSizeExpr _ n t1 t2 -> solveHasConLits     constraint n t1 t2
    _ -> compilerDeveloperError $ "Unknown type-class" <+> squotes (prettyVerbose eWHNF)

  unless (isStuck progress) $ do
    let primDict = PrimDict (annotationOf eWHNF) eWHNF
    let solution = abstractOver (boundContext constraint) primDict
    metaSolved (provenanceOf ctx) m solution

  return progress

solveTypeClassConstraint _ (_ `Has` e) =
  compilerDeveloperError $ "Unknown type-class application" <+> squotes (prettyVerbose e)

-- Takes in an expression and returns the list of non-inferable
-- meta variables contained within it.
getNonInferableMetas :: CheckedExpr -> [CheckedExpr]
getNonInferableMetas e =
  let recurse v = maybe [] getNonInferableMetas (getVisibleArg v) in
  case toHead e of
    (Builtin _ (TypeClass HasConOps),            [_tElem, tCont]) -> recurse tCont
    (Builtin _ (TypeClass (HasConLitsOfSize _)), [_tElem, tCont]) -> recurse tCont
    (f@Meta{} , _) -> [f]
    (_, args)      -> concatMap recurse args

getVisibleArg :: CheckedArg -> Maybe CheckedExpr
getVisibleArg (ExplicitArg _ arg) = Just arg
getVisibleArg _                   = Nothing

blockOnMetas :: MonadConstraintSolving m
             => CheckedExpr
             -> m ConstraintProgress
             -> m ConstraintProgress
blockOnMetas e solve = do
  let metas = getNonInferableMetas e
  if null metas
    then solve
    else do
      logDebug MaxDetail $ "stuck-on metas" <+> prettyVerbose metas
      return Stuck

progressByUnification :: MonadConstraintSolving m
                      => Constraint
                      -> [UnificationPair]
                      -> m ConstraintProgress
progressByUnification c pairs = return $ Progress
  { newConstraints = fmap (UC ctx . Unify) pairs
  , solvedMetas    = mempty
  } where ctx = (constraintContext c) { blockedBy = mempty }

solveHasEq :: MonadConstraintSolving m
           => Constraint
           -> CheckedExpr
           -> m ConstraintProgress
solveHasEq _ BoolType{}                  = return simplySolved
solveHasEq _ BuiltinNumericType{}        = return simplySolved
solveHasEq _ IndexType{}                 = return simplySolved
solveHasEq c (TensorType _ tElem _tDims) = solveHasEq c tElem
solveHasEq c (ListType _ tElem)          = solveHasEq c tElem
solveHasEq constraint _                  =
  throwError $ FailedConstraints (constraint :| [])

solveHasOrd :: MonadConstraintSolving m
            => Constraint
            -> CheckedExpr
            -> m ConstraintProgress
solveHasOrd _ IndexType{}          = return simplySolved
solveHasOrd _ BuiltinNumericType{} = return simplySolved
solveHasOrd constraint _           =
  throwError $ FailedConstraints (constraint :| [])

unifyTensorElems :: MonadConstraintSolving m
                 => Constraint
                 -> Maybe Int
                 -> CheckedExpr
                 -> CheckedExpr
                 -> CheckedExpr
                 -> m ConstraintProgress
unifyTensorElems c dim tElem tBaseElem (ConsExpr ann t [tDim, tDims]) =
  progressByUnification c $
    [ (tElem, TensorType ann tBaseElem (argExpr tDims)) ] <>
    maybeToList (fmap (\d -> (NatLiteralExpr ann t d, argExpr tDim)) dim)
unifyTensorElems c dim tElem tBaseElem (SeqExpr ann t1 t2 (tDim : tDims)) =
  progressByUnification c $
    [ (tElem, TensorType ann tBaseElem (SeqExpr ann t1 t2 tDims)) ] <>
    maybeToList (fmap (\d -> (NatLiteralExpr ann t1 d, tDim)) dim)
unifyTensorElems c _ _ _ _ = throwError $ FailedConstraints (c :| [])

solveHasConOps :: MonadConstraintSolving m
               => Constraint
               -> CheckedExpr
               -> CheckedExpr
               -> m ConstraintProgress
solveHasConOps c tElem1 (ListType   _ tElem2)          = progressByUnification c [(tElem1, tElem2)]
solveHasConOps c tElem  (TensorType _ tBaseElem tDims) = unifyTensorElems c Nothing tElem tBaseElem tDims
solveHasConOps c _ _ = throwError $ FailedConstraints (c :| [])

solveHasConLits :: MonadConstraintSolving m
                => Constraint
                -> Int
                -> CheckedExpr
                -> CheckedExpr
                -> m ConstraintProgress
solveHasConLits c _ tElem1 (ListType   _ tElem2)       = progressByUnification c [(tElem1, tElem2)]
solveHasConLits c n tElem1 (TensorType _ tElem2 tDims) = unifyTensorElems c (Just n) tElem1 tElem2 tDims
solveHasConLits c _ _ _ = throwError $ FailedConstraints (c :| [])

solveHasNatLits :: MonadConstraintSolving m
                => Constraint
                -> Int
                -> CheckedExpr
                -> m ConstraintProgress
solveHasNatLits _ value (IndexType _ (App _ (Literal _ (LNat n)) _))
-- ^ Can't pattern match on NatLitExpr here as the PrimDict may not yet have been solved.
  | value < n = return simplySolved
solveHasNatLits _ _   (NatType  _)  = return simplySolved
solveHasNatLits _ _   (IntType  _)  = return simplySolved
solveHasNatLits _ _   (RatType  _)  = return simplySolved
solveHasNatLits _ _   (RealType _)  = return simplySolved
solveHasNatLits constraint _ _    = throwError $ FailedConstraints (constraint :| [])

solveHasIntLits :: MonadConstraintSolving m
               => Constraint
               -> CheckedExpr
               -> m ConstraintProgress
solveHasIntLits _ (IntType  _) = return simplySolved
solveHasIntLits _ (RatType  _) = return simplySolved
solveHasIntLits _ (RealType _) = return simplySolved
solveHasIntLits constraint _   = throwError $ FailedConstraints (constraint :| [])

solveHasRatLits :: MonadConstraintSolving m
               => Constraint
               -> CheckedExpr
               -> m ConstraintProgress
solveHasRatLits _ (RatType  _) = return simplySolved
solveHasRatLits _ (RealType _) = return simplySolved
solveHasRatLits constraint _   = throwError $ FailedConstraints (constraint :| [])

solveHasNatOps :: MonadConstraintSolving m
               => Constraint
               -> CheckedExpr
               -> m ConstraintProgress
solveHasNatOps _ (NatType  _) = return simplySolved
solveHasNatOps _ (IntType  _) = return simplySolved
solveHasNatOps _ (RatType  _) = return simplySolved
solveHasNatOps _ (RealType _) = return simplySolved
solveHasNatOps constraint _ = throwError $ FailedConstraints (constraint :| [])

solveHasIntOps :: MonadConstraintSolving m
               => Constraint
               -> CheckedExpr
               -> m ConstraintProgress
solveHasIntOps _ (IntType  _) = return simplySolved
solveHasIntOps _ (RatType  _) = return simplySolved
solveHasIntOps _ (RealType _) = return simplySolved
solveHasIntOps constraint _ = throwError $ FailedConstraints (constraint :| [])

solveHasRatOps :: MonadConstraintSolving m
               => Constraint
               -> CheckedExpr
               -> m ConstraintProgress
solveHasRatOps _ (RatType  _) = return simplySolved
solveHasRatOps _ (RealType _) = return simplySolved
solveHasRatOps constraint _ = throwError $ FailedConstraints (constraint :| [])

simplySolved :: ConstraintProgress
simplySolved = Progress mempty mempty

abstractOver :: BoundCtx -> CheckedExpr -> CheckedExpr
abstractOver ctx body = foldr typeToLam body (fmap (\(_, t, _) -> t) ctx)
  where
    typeToLam :: CheckedExpr -> CheckedExpr -> CheckedExpr
    typeToLam t = Lam ann (ExplicitBinder ann Nothing t)
      where ann = annotationOf t


--------------------------------------------------------------------------------
-- Default solutions

-- This is some pretty ugly code. There must be a way of making this process
-- more elegant....

type Ctx = ConstraintContext

solveDefaultTypeClassConstraints :: MonadConstraintSolving m
                                 => [(TypeClassConstraint, Ctx)]
                                 -> m ConstraintProgress
solveDefaultTypeClassConstraints constraints = do
  -- First group by common meta-variables
  let constraintsByMeta = Map.mapMaybe id $ groupByMetas constraints
  newConstraints <- forM (Map.assocs constraintsByMeta) $ \(meta, (tc, (metaExpr, ctx))) -> do
    logDebug MaxDetail $ "Using default for" <+> pretty meta <+> "=" <+> pretty tc
    let ann = inserted $ provenanceOf ctx
    solution <- defaultSolution ann tc
    return $ UC ctx (Unify (metaExpr, solution))

  return $ if null newConstraints
    then Stuck
    else Progress newConstraints mempty

groupByMetas :: [(TypeClassConstraint, Ctx)]
             -> Map Meta (Maybe (TypeClass, (CheckedExpr, Ctx)))
groupByMetas []       = mempty
groupByMetas ((x, ctx) : xs) = case getDefaultCandidate x of
  Nothing      -> groupByMetas xs
  Just (m, mExpr, tc) -> Map.insertWith merge m (Just (tc, (mExpr, ctx))) (groupByMetas xs)
  where
    merge :: Maybe (TypeClass, a) -> Maybe (TypeClass, a) -> Maybe (TypeClass, a)
    merge (Just tc1) (Just tc2) = strongest tc1 tc2
    merge _          _          = Nothing

strongest :: (TypeClass, a) -> (TypeClass, a) -> Maybe (TypeClass, a)
strongest x@(tc1, _) y@(tc2, _) = case (numType tc1, numType tc2) of
  (Just c1, Just c2) -> Just $ if c1 > c2 then x else y
  _                  -> Nothing
  where
  numType :: TypeClass -> Maybe (NumericType, Bool, Int)
  numType HasNatOps          = Just (Nat, True,  0)
  numType HasIntOps          = Just (Int, True,  0)
  numType HasRatOps          = Just (Rat, True,  0)
  numType (HasNatLitsUpTo n) = Just (Nat, False, n)
  numType HasIntLits         = Just (Int, False, 0)
  numType HasRatLits         = Just (Rat, False, 0)
  numType _                  = Nothing

defaultSolution :: MonadCompile m => CheckedAnn -> TypeClass -> m CheckedExpr
defaultSolution ann HasNatOps          = return $ NatType ann
defaultSolution ann HasIntOps          = return $ IntType ann
defaultSolution ann HasRatOps          = return $ RatType ann
defaultSolution ann (HasNatLitsUpTo n) = return $ mkIndexType ann (n + 1)
defaultSolution ann HasIntLits         = return $ IntType ann
defaultSolution ann HasRatLits         = return $ RatType ann
defaultSolution _   tc                 = compilerDeveloperError $
  "TypeClass" <+> pretty tc <+> "should have already been eliminated"

getDefaultCandidate :: TypeClassConstraint -> Maybe (Meta, CheckedExpr, TypeClass)
getDefaultCandidate (_ `Has` e) = case e of
  HasNatLitsUpToExpr _ n t -> extractMeta t (HasNatLitsUpTo n)
  HasIntLitsExpr     _   t -> extractMeta t HasIntLits
  HasRatLitsExpr     _   t -> extractMeta t HasRatLits
  HasNatOpsExpr      _   t -> extractMeta t HasNatOps
  HasIntOpsExpr      _   t -> extractMeta t HasIntOps
  HasRatOpsExpr      _   t -> extractMeta t HasRatOps
  _                        -> Nothing

extractMeta :: CheckedExpr -> TypeClass -> Maybe (Meta, CheckedExpr, TypeClass)
extractMeta t tc = case exprHead t of
  (Meta _ m) -> Just (m, t, tc)
  _          -> Nothing