module Vehicle.Compile.Type.TypeClass
  ( solveTypeClassConstraint
  , solveDefaultTypeClassConstraints
  ) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad (unless, forM, when)
import Control.Monad.Except ( throwError )

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Language.Print (prettyVerbose)

--------------------------------------------------------------------------------
-- Solution

solveTypeClassConstraint :: MonadConstraintSolving m
                         => ConstraintContext
                         -> TypeClassConstraint
                         -> m ConstraintProgress
solveTypeClassConstraint ctx (m `Has` e) = do
  eWHNF <- whnf (varContext ctx) e
  when (e /= eWHNF) $
    logDebug ("normalising to" <+> prettyVerbose eWHNF)

  let constraint = TC ctx (m `Has` eWHNF)
  progress <- blockOnMetas eWHNF $ case eWHNF of
    IsContainerExpr      _ t1 t2 -> solveIsContainer    constraint t1 t2
    IsTruthExpr          _ t     -> solveIsTruth        constraint t
    HasEqExpr            _ t1 t2 -> solveHasEq          constraint t1 t2
    HasOrdExpr           _ t1 t2 -> solveHasOrd         constraint t1 t2
    HasNatOpsExpr        _ t     -> solveHasNatOps      constraint t
    HasIntOpsExpr        _ t     -> solveHasIntOps      constraint t
    HasRatOpsExpr        _ t     -> solveHasRatOps      constraint t
    HasNatLitsUpToExpr   _ n t   -> solveHasNatLits     constraint n t
    HasIntLitsExpr       _ t     -> solveHasIntLits     constraint t
    HasRatLitsExpr       _ t     -> solveHasRatLits     constraint t
    _ -> developerError $ "Unknown type-class" <+> squotes (prettyVerbose eWHNF)

  unless (isStuck progress) $ do
    let primDict = PrimDict (annotationOf eWHNF) eWHNF
    let solution = abstractOver (boundContext constraint) primDict
    metaSolved (provenanceOf ctx) m solution

  return progress

-- Takes in an expression and returns the list of non-inferable
-- meta variables contained within it.
getNonInferableMetas :: CheckedExpr -> [CheckedExpr]
getNonInferableMetas e =
  let recurse v = maybe [] getNonInferableMetas (getVisibleArg v) in
  case toHead e of
    (f@Meta{} , _)                                       -> [f]
    (Builtin _ (TypeClass IsContainer), [_tElem, tCont]) -> recurse tCont
    (_, args)                                            -> concatMap recurse args

getVisibleArg :: CheckedArg -> Maybe CheckedExpr
getVisibleArg (ExplicitArg _ arg) = Just arg
getVisibleArg _                   = Nothing

blockOnMetas :: MonadConstraintSolving m => CheckedExpr -> m ConstraintProgress -> m ConstraintProgress
blockOnMetas e action = do
  let metas = getNonInferableMetas e
  if null metas
    then action
    else do
      logDebug $ "stuck-on metas" <+> prettyVerbose metas
      return Stuck

solveHasEq :: MonadConstraintSolving m
           => Constraint
           -> CheckedExpr
           -> CheckedExpr
           -> m ConstraintProgress
solveHasEq _ (BuiltinBooleanType _ _)    (BuiltinBooleanType _ _) = return simplySolved
solveHasEq _ (BuiltinNumericType _ _)    (BoolType _)             = return simplySolved
solveHasEq _ (BuiltinNumericType _ t)    (PropType _)
  | isDecidable t = return simplySolved
solveHasEq _ (FinType _ _)               _    = return simplySolved
solveHasEq c (TensorType _ tElem _tDims) tRes = solveHasEq c tElem tRes
solveHasEq c (ListType _ tElem)          tRes = solveHasEq c tElem tRes
solveHasEq constraint _ _ = throwError $ FailedConstraints (constraint :| [])

solveHasOrd :: MonadConstraintSolving m
            => Constraint
            -> CheckedExpr
            -> CheckedExpr
            -> m ConstraintProgress
solveHasOrd _ (FinType _ _)            _            = return simplySolved
solveHasOrd _ (BuiltinNumericType _ _) (PropType _) = return simplySolved
solveHasOrd _ (BuiltinNumericType _ t) (BoolType _)
  | isDecidable t = return simplySolved
solveHasOrd constraint _ _ = throwError $ FailedConstraints (constraint :| [])

solveIsTruth :: MonadConstraintSolving m
             => Constraint
             -> CheckedExpr
             -> m ConstraintProgress
solveIsTruth _ (BuiltinBooleanType _ _) = return simplySolved
solveIsTruth constraint _ = throwError $ FailedConstraints (constraint :| [])

solveIsContainer :: MonadConstraintSolving m
                 => Constraint
                 -> CheckedExpr
                 -> CheckedExpr
                 -> m ConstraintProgress
solveIsContainer c tElem tCont =
  return $ case getContainerElem tCont of
    Nothing -> Stuck
    Just t  -> Progress
      { newConstraints = [UC ctx (Unify (tElem, t))]
      , solvedMetas    = mempty
      }
  where ctx = ConstraintContext (provenanceOf c) mempty (variableContext c)


solveHasNatLits :: MonadConstraintSolving m
                => Constraint
                -> Int
                -> CheckedExpr
                -> m ConstraintProgress
solveHasNatLits _ value (FinType _ (NatLiteralExpr _ _ n))
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
  newConstraints <- forM (Map.assocs constraintsByMeta) $ \(meta, (tc, ctx)) -> do
    logDebug $ "Using default for" <+> pretty meta <+> "=" <+> pretty tc
    let ann = inserted $ provenanceOf ctx
    let solution = defaultSolution ann tc
    return $ UC ctx (Unify (Meta ann meta, solution))

  return $ if null newConstraints
    then Stuck
    else Progress newConstraints mempty

groupByMetas :: [(TypeClassConstraint, Ctx)]
             -> Map Meta (Maybe (TypeClass, Ctx))
groupByMetas []       = mempty
groupByMetas ((x, ctx) : xs) = case getDefaultCandidate x of
  Nothing      -> groupByMetas xs
  Just (m, tc) -> Map.insertWith merge m (Just (tc, ctx)) (groupByMetas xs)
  where
    merge :: Maybe (TypeClass, Ctx) -> Maybe (TypeClass, Ctx) -> Maybe (TypeClass, Ctx)
    merge (Just tc1) (Just tc2) = strongest tc1 tc2
    merge _          _          = Nothing

strongest :: (TypeClass, Ctx) -> (TypeClass, Ctx) -> Maybe (TypeClass, Ctx)
strongest x@(tc1, _) y@(tc2, _) = case (numType tc1, numType tc2) of
  (Just (t1, isOp1), Just (t2, isOp2))
    | t1 < t2       -> Just y
    | t2 < t1       -> Just x
    | isOp1 < isOp2 -> Just y
    | otherwise     -> Just x
  _ -> Nothing
  where
  numType :: TypeClass -> Maybe (NumericType, Bool)
  numType HasNatOps        = Just (Nat, True)
  numType HasIntOps        = Just (Int, True)
  numType HasRatOps        = Just (Rat, True)
  numType HasNatLitsUpTo{} = Just (Nat, False)
  numType HasIntLits       = Just (Int, False)
  numType HasRatLits       = Just (Rat, False)
  numType _                = Nothing

defaultSolution :: CheckedAnn -> TypeClass -> CheckedExpr
defaultSolution ann HasNatOps          = NatType ann
defaultSolution ann HasIntOps          = IntType ann
defaultSolution ann HasRatOps          = RatType ann
defaultSolution ann (HasNatLitsUpTo n) = FinType ann (NatLiteralExpr ann (NatType ann) (n + 1))
defaultSolution ann HasIntLits         = IntType ann
defaultSolution ann HasRatLits         = RatType ann
defaultSolution _   tc                 =
  developerError $ "TypeClass" <+> pretty (show tc) <+> "should have already been eliminated"

getDefaultCandidate :: TypeClassConstraint -> Maybe (Meta, TypeClass)
getDefaultCandidate (_ `Has` e) = case e of
  (HasNatLitsUpToExpr _ n (Meta _ m)) -> Just (m , HasNatLitsUpTo n)
  (HasIntLitsExpr     _   (Meta _ m)) -> Just (m , HasIntLits)
  (HasRatLitsExpr     _   (Meta _ m)) -> Just (m , HasRatLits)
  (HasNatOpsExpr      _   (Meta _ m)) -> Just (m , HasNatOps)
  (HasIntOpsExpr      _   (Meta _ m)) -> Just (m , HasIntOps)
  (HasRatOpsExpr      _   (Meta _ m)) -> Just (m , HasRatOps)
  _                                   -> Nothing

