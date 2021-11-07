{-# LANGUAGE ViewPatterns #-}

module Vehicle.Language.Type.TypeClass
  ( solveTypeClassConstraint
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad (unless)
import Control.Monad.Except ( throwError )

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Type.Core
import Vehicle.Language.Type.Meta
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
  eWHNF <- whnf (declContext ctx) e
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

  unless (isStuck progress) $
    metaSolved (provenanceOf ctx) m eWHNF

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

isMeta :: CheckedExpr -> Bool
isMeta (Meta _ _)           = True
isMeta (App _ (Meta _ _) _) = True
isMeta _                    = False

solveHasEq :: MonadConstraintSolving m
           => Constraint
           -> CheckedExpr
           -> CheckedExpr
           -> m ConstraintProgress
-- TODO insert Container classes
solveHasEq _ (BuiltinBooleanType _ _)  (BuiltinBooleanType _ _) = return simplySolved
solveHasEq _ (BuiltinNumericType _ _)  (BuiltinBooleanType _ _) = return simplySolved
solveHasEq constraint _ _ = throwError $ FailedConstraints (constraint :| [])

solveHasOrd :: MonadConstraintSolving m
            => Constraint
            -> CheckedExpr
            -> CheckedExpr
            -> m ConstraintProgress
solveHasOrd _ (BuiltinNumericType _ _) (BuiltinBooleanType _ Prop) = return simplySolved
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
solveIsContainer c tElem tCont = do
  tContElem <- getContainerElem tCont
  return $ case tContElem of
    Nothing -> Stuck
    Just t  -> Progress
      { newConstraints = [Constraint (ConstraintContext (provenanceOf c) mempty (variableContext c)) (Unify (tElem, t))]
      , solvedMetas    = mempty
      }
  where
    getContainerElem :: MonadConstraintSolving m => CheckedExpr -> m (Maybe CheckedExpr)
    getContainerElem t = case toHead t of
      (Builtin _ List,   [tElem'])    -> return $ Just $ argExpr tElem'
      (Builtin _ Tensor, [tElem', _]) -> return $ Just $ argExpr tElem'
      _                               -> return Nothing

solveIsNatural :: MonadConstraintSolving m
               => Constraint
               -> CheckedExpr
               -> m ConstraintProgress
solveIsNatural _ (BuiltinNumericType _ Nat)  = return simplySolved
solveIsNatural _ (BuiltinNumericType _ Int)  = return simplySolved
solveIsNatural _ (BuiltinNumericType _ Real) = return simplySolved
solveIsNatural constraint _ = throwError $ FailedConstraints (constraint :| [])

solveIsIntegral :: MonadConstraintSolving m
             => Constraint
             -> CheckedExpr
             -> m ConstraintProgress
solveIsIntegral _ (BuiltinNumericType _ Int)  = return simplySolved
solveIsIntegral _ (BuiltinNumericType _ Real) = return simplySolved
solveIsIntegral constraint _ = throwError $ FailedConstraints (constraint :| [])

solveIsRational :: MonadConstraintSolving m
             => Constraint
             -> CheckedExpr
             -> m ConstraintProgress
solveIsRational _ (BuiltinNumericType _ Real) = return simplySolved
solveIsRational constraint _ = throwError $ FailedConstraints (constraint :| [])

solveIsReal :: MonadConstraintSolving m
            => Constraint
            -> CheckedExpr
            -> m ConstraintProgress
solveIsReal _ (BuiltinNumericType _ Real) = return simplySolved
solveIsReal constraint _ = throwError $ FailedConstraints (constraint :| [])

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