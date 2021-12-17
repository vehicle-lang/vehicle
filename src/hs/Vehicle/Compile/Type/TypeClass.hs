{-# LANGUAGE ViewPatterns #-}

module Vehicle.Compile.Type.TypeClass
  ( solveTypeClassConstraint
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad (unless)
import Control.Monad.Except ( throwError )

import Vehicle.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.AST
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Type.Core

--------------------------------------------------------------------------------
-- Solution

-- To solve / synthesise typeClassConstraints :
--  1. Normalise each one in the current metactxt
--  2. Insert lambdas for each Pi type
--  3. Match against (a) the list of built-in instances; then (b) against
--     elements in the context
--  4. If the constraint is a Z3 one, then ask Z3 to solve it (after normalisation)


solveTypeClassConstraint :: MonadConstraintSolving e m
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
        (IsNatural,      [t])      -> solveIsNatural      constraint t
        (IsIntegral,     [t])      -> solveIsIntegral     constraint t
        (IsRational,     [t])      -> solveIsRational     constraint t
        (IsReal,         [t])      -> solveIsReal         constraint t
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

blockOnMetas :: MonadConstraintSolving e m => TypeClass -> [CheckedExpr] -> m ConstraintProgress -> m ConstraintProgress
blockOnMetas tc args action = do
  let metas = filter isMeta (getNonInferableArgs tc args)
  if null metas
    then action
    else do
      logDebug $ "stuck-on metas" <+> prettyVerbose metas
      return Stuck

solveHasEq :: MonadConstraintSolving e m
           => Constraint
           -> CheckedExpr
           -> CheckedExpr
           -> m ConstraintProgress
-- TODO insert Container classes
solveHasEq _ (BuiltinBooleanType _ _)  (BuiltinBooleanType _ _) = return simplySolved
solveHasEq _ (BuiltinNumericType _ _)  (BoolType _)             = return simplySolved
solveHasEq _ (BuiltinNumericType _ t)  (PropType _)
  | isDecidable t = return simplySolved
solveHasEq constraint _ _ = throwError $ mkFailedConstraints (constraint :| [])

solveHasOrd :: MonadConstraintSolving e m
            => Constraint
            -> CheckedExpr
            -> CheckedExpr
            -> m ConstraintProgress
solveHasOrd _ (BuiltinNumericType _ _) (PropType _) = return simplySolved
solveHasOrd _ (BuiltinNumericType _ t) (BoolType _)
  | isDecidable t = return simplySolved
solveHasOrd constraint _ _ = throwError $ mkFailedConstraints (constraint :| [])

solveIsTruth :: MonadConstraintSolving e m
             => Constraint
             -> CheckedExpr
             -> m ConstraintProgress
solveIsTruth _ (BuiltinBooleanType _ _) = return simplySolved
solveIsTruth constraint _ = throwError $ mkFailedConstraints (constraint :| [])

solveIsContainer :: MonadConstraintSolving e m
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
    getContainerElem t = case toHead t of
      (BuiltinContainerType _ List,   [tElem'])    -> Just $ argExpr tElem'
      (BuiltinContainerType _ Tensor, [tElem', _]) -> Just $ argExpr tElem'
      _ -> Nothing

solveIsNatural :: MonadConstraintSolving e m
               => Constraint
               -> CheckedExpr
               -> m ConstraintProgress
solveIsNatural _ (NatType  _) = return simplySolved
solveIsNatural _ (IntType  _) = return simplySolved
solveIsNatural _ (RatType  _) = return simplySolved
solveIsNatural _ (RealType _) = return simplySolved
solveIsNatural constraint _ = throwError $ mkFailedConstraints (constraint :| [])

solveIsIntegral :: MonadConstraintSolving e m
             => Constraint
             -> CheckedExpr
             -> m ConstraintProgress
solveIsIntegral _ (IntType  _) = return simplySolved
solveIsIntegral _ (RatType  _) = return simplySolved
solveIsIntegral _ (RealType _) = return simplySolved
solveIsIntegral constraint _ = throwError $ mkFailedConstraints (constraint :| [])

solveIsRational :: MonadConstraintSolving e m
             => Constraint
             -> CheckedExpr
             -> m ConstraintProgress
solveIsRational _ (RatType  _) = return simplySolved
solveIsRational _ (RealType _) = return simplySolved
solveIsRational constraint _ = throwError $ mkFailedConstraints (constraint :| [])

solveIsReal :: MonadConstraintSolving e m
            => Constraint
            -> CheckedExpr
            -> m ConstraintProgress
solveIsReal _ (RealType _) = return simplySolved
solveIsReal constraint _ = throwError $ mkFailedConstraints (constraint :| [])

solveIsQuantifiable :: MonadConstraintSolving e m
                    => Constraint
                    -> CheckedExpr
                    -> CheckedExpr
                    -> m ConstraintProgress
-- TODO
solveIsQuantifiable constraint _ _ =
  throwError $ mkFailedConstraints (constraint :| [])

simplySolved :: ConstraintProgress
simplySolved = Progress mempty mempty

abstractOver :: BoundCtx -> CheckedExpr -> CheckedExpr
abstractOver ctx body = foldr typeToLam body (fmap (\(_, t, _) -> t) ctx)
  where
    typeToLam :: CheckedExpr -> CheckedExpr -> CheckedExpr
    typeToLam t = Lam ann (ExplicitBinder ann Nothing t)
      where ann = annotationOf t