module Vehicle.Compile.Type.TypeClass
  ( solveTypeClassConstraint
  ) where

import Data.Maybe ( maybeToList )
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad (unless)
import Control.Monad.Except ( throwError )

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.WeakHeadNormalForm
import Vehicle.Language.Print (prettyVerbose)

--------------------------------------------------------------------------------
-- Public interface

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
    HasAddExpr           _ t       -> solveHasAdd         constraint t
    HasSubExpr           _ t       -> solveHasSub         constraint t
    HasMulExpr           _ t       -> solveHasMul         constraint t
    HasDivExpr           _ t       -> solveHasDiv         constraint t
    HasNegExpr           _ t       -> solveHasNeg         constraint t
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

--------------------------------------------------------------------------------
-- Solving methods

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

solveHasAdd :: MonadConstraintSolving m
            => Constraint
            -> CheckedExpr
            -> m ConstraintProgress
solveHasAdd _ (NatType  _) = return simplySolved
solveHasAdd _ (IntType  _) = return simplySolved
solveHasAdd _ (RatType  _) = return simplySolved
solveHasAdd _ (RealType _) = return simplySolved
solveHasAdd constraint _ = throwError $ FailedConstraints (constraint :| [])

solveHasSub :: MonadConstraintSolving m
               => Constraint
               -> CheckedExpr
               -> m ConstraintProgress
solveHasSub _ (IntType  _) = return simplySolved
solveHasSub _ (RatType  _) = return simplySolved
solveHasSub _ (RealType _) = return simplySolved
solveHasSub constraint _ = throwError $ FailedConstraints (constraint :| [])

solveHasMul :: MonadConstraintSolving m
            => Constraint
            -> CheckedExpr
            -> m ConstraintProgress
solveHasMul _ (NatType  _) = return simplySolved
solveHasMul _ (IntType  _) = return simplySolved
solveHasMul _ (RatType  _) = return simplySolved
solveHasMul _ (RealType _) = return simplySolved
solveHasMul constraint _ = throwError $ FailedConstraints (constraint :| [])

solveHasDiv :: MonadConstraintSolving m
            => Constraint
            -> CheckedExpr
            -> m ConstraintProgress
solveHasDiv _ (RatType  _) = return simplySolved
solveHasDiv _ (RealType _) = return simplySolved
solveHasDiv constraint _ = throwError $ FailedConstraints (constraint :| [])

solveHasNeg :: MonadConstraintSolving m
            => Constraint
            -> CheckedExpr
            -> m ConstraintProgress
solveHasNeg _ (IntType  _) = return simplySolved
solveHasNeg _ (RatType  _) = return simplySolved
solveHasNeg _ (RealType _) = return simplySolved
solveHasNeg constraint _   = throwError $ FailedConstraints (constraint :| [])

--------------------------------------------------------------------------------
-- Utilities

-- Takes in an expression and returns the list of expressions that cannot be
-- meta-variables if we are to make progress on the constraint.
getNonInferableExprs :: CheckedExpr -> [CheckedExpr]
getNonInferableExprs e = case toHead e of
  -- List constraints can always make progress as long as the container type
  -- itself isn't a meta-variable (although it may contain meta-variables).
  (BuiltinTypeClass _ HasConOps,            [_tElem, tCont]) -> [argExpr tCont]
  (BuiltinTypeClass _ (HasConLitsOfSize _), [_tElem, tCont]) -> [argExpr tCont]
  -- If the current expression itself is a meta, then simply return it.
  (f@Meta{} , _) -> [f]
  -- For any other type-class if any of the args contain meta-variables then
  -- they cannot be solved.
  (_, args)      -> concatMap (maybe [] getNonInferableExprs . getVisibleArg) args

getVisibleArg :: CheckedArg -> Maybe CheckedExpr
getVisibleArg (ExplicitArg _ arg) = Just arg
getVisibleArg _                   = Nothing

blockOnMetas :: MonadConstraintSolving m
             => CheckedExpr
             -> m ConstraintProgress
             -> m ConstraintProgress
blockOnMetas expr solve = do
  let metas = filter isMeta (getNonInferableExprs expr)
  if null metas
    then solve
    else do
      logDebug MaxDetail $ "stuck-on metas" <+> prettyVerbose metas
      return Stuck

simplySolved :: ConstraintProgress
simplySolved = Progress mempty mempty

progressByUnification :: MonadConstraintSolving m
                      => Constraint
                      -> [UnificationPair]
                      -> m ConstraintProgress
progressByUnification c pairs = return $ Progress
  { newConstraints = fmap (UC ctx . Unify) pairs
  , solvedMetas    = mempty
  } where ctx = (constraintContext c) { blockedBy = mempty }

abstractOver :: BoundCtx -> CheckedExpr -> CheckedExpr
abstractOver ctx body = foldr typeToLam body (fmap (\(_, t, _) -> t) ctx)
  where
    typeToLam :: CheckedExpr -> CheckedExpr -> CheckedExpr
    typeToLam t = Lam ann (ExplicitBinder ann Nothing t)
      where ann = annotationOf t


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