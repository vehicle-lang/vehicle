
module Vehicle.Core.Compile.Type.TypeClass
  ( solveTypeClassConstraint
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad (unless)
import Control.Monad.Except ( throwError )

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Compile.Type.Core
import Vehicle.Core.Compile.Type.Meta
import Vehicle.Core.Compile.Type.Normalise ( whnf )
import Vehicle.Core.Print (prettyVerbose)

--------------------------------------------------------------------------------
-- Solution

-- To solve / synthesise typeClassConstraints :
--  1. Normalise each one in the current metactxt
--  2. Insert lambdas for each Pi type
--  3. Match against (a) the list of built-in instances; then (b) against
--     elements in the context
--  4. If the constraint is a Z3 one, then ask Z3 to solve it (after normalisation)


solveTypeClassConstraint :: MonadConstraintSolving m
                         => Constraint
                         -> Meta
                         -> CheckedExpr
                         -> m ConstraintProgress
solveTypeClassConstraint c m e = do
  eWHNF <- whnf (declContext c) e
  progress <- case decomposeApp eWHNF of
    (Builtin _ tc, args) -> do
      let argsNF = extractArg <$> args
      blockOnMetas tc argsNF $ case (tc, argsNF) of
        (IsContainer,    [t1, t2]) -> solveIsContainer    c t1 t2
        (HasEq,          [t1, t2]) -> solveHasEq          c t1 t2
        (HasOrd,         [t1, t2]) -> solveHasOrd         c t1 t2
        (IsTruth,        [t])      -> solveIsTruth        c t
        (IsNatural,      [t])      -> solveIsNatural      c t
        (IsIntegral,     [t])      -> solveIsIntegral     c t
        (IsRational,     [t])      -> solveIsRational     c t
        (IsReal,         [t])      -> solveIsReal         c t
        (IsQuantifiable, [t1, t2]) -> solveIsQuantifiable c t1 t2
        _                          -> developerError $
          "Unknown type-class" <+> squotes (pretty tc) <+> "args" <+> prettyVerbose argsNF
    _ -> developerError $ "Unknown type-class" <+> squotes (prettyVerbose eWHNF)

  unless (isStuck progress) $
    metaSolved (prov c) m eWHNF

  return progress

-- Takes in the type-class and the list of arguments and returns the list of those
-- that cannot be inferred from the others.
getNonInferableArgs :: Builtin -> [CheckedExpr] -> [CheckedExpr]
getNonInferableArgs IsContainer [tCont, _tElem] = [tCont]
getNonInferableArgs _           args            = args

extractArg :: CheckedArg -> CheckedExpr
extractArg (Arg _ Explicit e) = e
extractArg _ = developerError "Not expecting type-classes with non-explicit arguments"

blockOnMetas :: MonadConstraintSolving m => Builtin -> [CheckedExpr] -> m ConstraintProgress -> m ConstraintProgress
blockOnMetas tc args action = do
  let metas = filter isMeta (getNonInferableArgs tc args)
  if null metas
    then action
    else do
      logDebug $ "stuck-on metas" <+> prettyVerbose metas
      return Stuck

isMeta :: CheckedExpr -> Bool
isMeta e = case decomposeApp e of
  (Meta _ _, _) -> True
  _             -> False

solveHasEq :: MonadConstraintSolving m
           => Constraint
           -> CheckedExpr
           -> CheckedExpr
           -> m ConstraintProgress
-- TODO insert Container classes
solveHasEq _          (Builtin _ Bool)  (Builtin _ Prop) = return simplySolved
solveHasEq _          (Builtin _ Prop)  (Builtin _ Prop) = return simplySolved
solveHasEq _          (Builtin _ Nat)   (Builtin _ Prop) = return simplySolved
solveHasEq _          (Builtin _ Int)   (Builtin _ Prop) = return simplySolved
solveHasEq _          (Builtin _ Real)  (Builtin _ Prop) = return simplySolved
solveHasEq _          (Builtin _ Bool)  (Builtin _ Bool) = return simplySolved
solveHasEq _          (Builtin _ Prop)  (Builtin _ Bool) = return simplySolved
solveHasEq _          (Builtin _ Nat)   (Builtin _ Bool) = return simplySolved
solveHasEq _          (Builtin _ Int)   (Builtin _ Bool) = return simplySolved
solveHasEq _          (Builtin _ Real)  (Builtin _ Bool) = return simplySolved
solveHasEq constraint _                 _                =
  throwError $ FailedConstraints (constraint :| [])

solveHasOrd :: MonadConstraintSolving m
            => Constraint
            -> CheckedExpr
            -> CheckedExpr
            -> m ConstraintProgress
solveHasOrd _          (Builtin _ Nat)  (Builtin _ Prop) = return simplySolved
solveHasOrd _          (Builtin _ Int)  (Builtin _ Prop) = return simplySolved
solveHasOrd _          (Builtin _ Real) (Builtin _ Prop) = return simplySolved
solveHasOrd constraint _                _                =
  throwError $ FailedConstraints (constraint :| [])

solveIsTruth :: MonadConstraintSolving m
             => Constraint
             -> CheckedExpr
             -> m ConstraintProgress
solveIsTruth _          (Builtin _ Bool) = return simplySolved
solveIsTruth _          (Builtin _ Prop) = return simplySolved
solveIsTruth constraint _                =
  throwError $ FailedConstraints (constraint :| [])

solveIsContainer :: MonadConstraintSolving m
                 => Constraint
                 -> CheckedExpr
                 -> CheckedExpr
                 -> m ConstraintProgress
solveIsContainer c tCont tElem = do
  tContElem <- getContainerElem tCont
  return $ case tContElem of
    Nothing -> Stuck
    Just t  -> Progress
      { newConstraints = [Constraint (ConstraintContext (prov c) mempty (variableContext c)) (Unify (tElem, t))]
      , solvedMetas    = mempty
      }
  where
    getContainerElem :: MonadConstraintSolving m => CheckedExpr -> m (Maybe CheckedExpr)
    getContainerElem t = case decomposeApp t of
      (Builtin _ List,   [tElem'])    -> return $ Just $ argExpr tElem'
      (Builtin _ Tensor, [tElem', _]) -> return $ Just $ argExpr tElem'
      _                               -> return Nothing

solveIsNatural :: MonadConstraintSolving m
               => Constraint
               -> CheckedExpr
               -> m ConstraintProgress
solveIsNatural _          (Builtin _ Nat)  = return simplySolved
solveIsNatural _          (Builtin _ Int)  = return simplySolved
solveIsNatural _          (Builtin _ Real) = return simplySolved
solveIsNatural constraint _                =
  throwError $ FailedConstraints (constraint :| [])

solveIsIntegral :: MonadConstraintSolving m
             => Constraint
             -> CheckedExpr
             -> m ConstraintProgress
solveIsIntegral _          (Builtin _ Int)  = return simplySolved
solveIsIntegral _          (Builtin _ Real) = return simplySolved
solveIsIntegral constraint  _               =
  throwError $ FailedConstraints (constraint :| [])

solveIsRational :: MonadConstraintSolving m
             => Constraint
             -> CheckedExpr
             -> m ConstraintProgress
solveIsRational _          (Builtin _ Real) = return simplySolved
solveIsRational constraint _                =
  throwError $ FailedConstraints (constraint :| [])

solveIsReal :: MonadConstraintSolving m
            => Constraint
            -> CheckedExpr
            -> m ConstraintProgress
solveIsReal _          (Builtin _ Real) = return simplySolved
solveIsReal constraint _                =
  throwError $ FailedConstraints (constraint :| [])

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