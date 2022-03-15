module Vehicle.Compile.Type.TypeClass
  ( solveTypeClassConstraint
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad (unless)
import Control.Monad.Except ( throwError )

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Language.Print (prettyVerbose)

--------------------------------------------------------------------------------
-- Solution

-- To solve / synthesise typeClassConstraints :
--  1. Normalise each one in the current metactxt
--  2. Insert lambdas for each Pi type
--  3. Match against (a) the list of built-in instances; then (b) against
--     elements in the context
--  4. If the constraint is a Z3 one, then ask Z3 to solve it (after normalisation)


solveTypeClassConstraint :: MonadConstraintSolving m
                         => ConstraintContext
                         -> Meta
                         -> CheckedExpr
                         -> m ConstraintProgress
solveTypeClassConstraint ctx m e = do
  eWHNF <- whnf (varContext ctx) e
  let constraint = Constraint ctx (m `Has` eWHNF)
  progress <- case toHead eWHNF of
    (Builtin _ (TypeClass tc), args) -> do
      let argsNF = extractArg <$> args
      blockOnMetas tc argsNF $ case (tc, argsNF) of
        (IsContainer,    [t1, t2]) -> solveIsContainer    constraint t1 t2
        (HasEq,          [t1, t2]) -> solveHasEq          constraint t1 t2
        (HasOrd,         [t1, t2]) -> solveHasOrd         constraint t1 t2
        (IsTruth,        [t])      -> solveIsTruth        constraint t
        (HasNatOps,      [t])      -> solveHasNatOps      constraint t
        (HasIntOps,      [t])      -> solveHasIntOps      constraint t
        (HasRatOps,      [t])      -> solveHasRatOps      constraint t
        (HasNatLits,     [t])      -> solveHasNatLits     constraint t
        (HasIntLits,     [t])      -> solveHasIntLits     constraint t
        (HasRatLits,     [t])      -> solveHasRatLits     constraint t
        (IsQuantifiable, [t1, t2]) -> solveIsQuantifiable constraint t1 t2
        _                          -> developerError $
          "Unknown type-class" <+> squotes (pretty (show tc)) <+> "args" <+> prettyVerbose argsNF
    _ -> developerError $ "Unknown type-class" <+> squotes (prettyVerbose eWHNF)

  unless (isStuck progress) $ do
    let primDict = PrimDict (annotationOf eWHNF) eWHNF
    let solution = abstractOver (boundContext constraint) primDict
    metaSolved (provenanceOf ctx) m solution

  return progress

-- Takes in the type-class and the list of arguments and returns the list of those
-- that cannot be inferred from the others.
getNonInferableArgs :: TypeClass -> [CheckedExpr] -> [CheckedExpr]
getNonInferableArgs IsContainer [_tElem, tCont] = [tCont]
getNonInferableArgs _           args            = args

extractArg :: CheckedArg -> CheckedExpr
extractArg arg = if visibilityOf arg == Explicit
  then argExpr arg
  else developerError "Not expecting type-classes with non-explicit arguments"

blockOnMetas :: MonadConstraintSolving m => TypeClass -> [CheckedExpr] -> m ConstraintProgress -> m ConstraintProgress
blockOnMetas tc args action = do
  let metas = filter isMeta (getNonInferableArgs tc args)
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
-- TODO insert Container classes
solveHasEq _ (BuiltinBooleanType _ _)    (BuiltinBooleanType _ _) = return simplySolved
solveHasEq _ (BuiltinNumericType _ _)    (BoolType _)             = return simplySolved
solveHasEq _ (BuiltinNumericType _ t)    (PropType _)
  | isDecidable t = return simplySolved
solveHasEq c (TensorType _ tElem _tDims) tRes = solveHasEq c tElem tRes
solveHasEq c (ListType _ tElem)          tRes = solveHasEq c tElem tRes
solveHasEq constraint _ _ = throwError $ FailedConstraints (constraint :| [])

solveHasOrd :: MonadConstraintSolving m
            => Constraint
            -> CheckedExpr
            -> CheckedExpr
            -> m ConstraintProgress
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
      { newConstraints = [Constraint (ConstraintContext (provenanceOf c) mempty (variableContext c)) (Unify (tElem, t))]
      , solvedMetas    = mempty
      }
  where
    getContainerElem :: CheckedExpr -> Maybe CheckedExpr
    getContainerElem (ListType   _ t)   = Just t
    getContainerElem (TensorType _ t _) = Just t
    getContainerElem _                  = Nothing

solveHasNatLits :: MonadConstraintSolving m
               => Constraint
               -> CheckedExpr
               -> m ConstraintProgress
solveHasNatLits _ (NatType  _) = return simplySolved
solveHasNatLits _ (IntType  _) = return simplySolved
solveHasNatLits _ (RatType  _) = return simplySolved
solveHasNatLits _ (RealType _) = return simplySolved
solveHasNatLits constraint _   = throwError $ FailedConstraints (constraint :| [])

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

solveIsQuantifiable :: MonadConstraintSolving m
                    => Constraint
                    -> CheckedExpr
                    -> CheckedExpr
                    -> m ConstraintProgress
-- TODO
solveIsQuantifiable constraint _ _ =
  throwError $ FailedConstraints (constraint :| [])

simplySolved :: ConstraintProgress
simplySolved = Progress mempty mempty

abstractOver :: BoundCtx -> CheckedExpr -> CheckedExpr
abstractOver ctx body = foldr typeToLam body (fmap (\(_, t, _) -> t) ctx)
  where
    typeToLam :: CheckedExpr -> CheckedExpr -> CheckedExpr
    typeToLam t = Lam ann (ExplicitBinder ann Nothing t)
      where ann = annotationOf t