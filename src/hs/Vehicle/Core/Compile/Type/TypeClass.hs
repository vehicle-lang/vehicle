
module Vehicle.Core.Compile.Type.TypeClass
  ( solveTypeClassConstraints
  )
  where

import Data.Functor ((<&>))
import Control.Monad.Except ( MonadError, throwError )
import Prettyprinter ( (<+>), Pretty(pretty), squotes )

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Compile.Type.Core
import Vehicle.Core.Compile.Type.Meta
import Vehicle.Core.Compile.Type.WHNF ( whnf )
import Vehicle.Core.Print.Core ()

--------------------------------------------------------------------------------
-- Solution

-- To solve / synthesise typeClassConstraints :
--  1. Normalise each one in the current metactxt
--  2. Insert lambdas for each Pi type
--  3. Match against (a) the list of built-in instances; then (b) against elements in the context
--  4. If the constraint is a Z3 one, then ask Z3 to solve it (after normalisation)

type MonadTCResolution m =
  ( MonadLogger m
  , MonadMeta m
  , MonadError TypingError m
  )

-- | Tries to solve the provided list of constraints, returning
-- whether or not any progress was made.
solveTypeClassConstraints :: MonadTCResolution m => m Bool
solveTypeClassConstraints = do
  logDebug "Starting new type-class resolution pass"

  constraints <- getTypeClassConstraints
  setTypeClassConstraints []

  constraints' <- substMetas <$> getMetaSubstitution <*> pure constraints

  logDebug $ "constraints:" <+> pretty constraints'

  progress <- mconcat `fmap` traverse solveConstraint constraints'

  subst <- getMetaSubstitution
  logDebug $ "current-solution:" <+> pretty subst <+> "\n"

  return $ not (isStuck progress)

solveConstraint :: MonadTCResolution m
                => TypeClassConstraint
                -> m Progress
solveConstraint constraint@(m `Has` e) = do
  logDebug $ "trying" <+> pretty constraint
  incrCallDepth
  result <- findInstance constraint e
  case result of
    Solved _ -> metaSolved (prov e) m (PrimDict e)
    _        -> addTypeClassConstraint constraint
  decrCallDepth
  return result

-- Takes in the type-class and the list of arguments and returns the list of those
-- that cannot be inferred from the others.
getNonInferableArgs :: Builtin -> [CheckedExpr] -> [CheckedExpr]
getNonInferableArgs IsContainer [tCont, _tElem] = [tCont]
getNonInferableArgs _           args            = args

findInstance :: MonadTCResolution m => TypeClassConstraint -> CheckedExpr -> m Progress
findInstance c e = do
  eWHNF <- whnf e
  case decomposeApp eWHNF of
    (Builtin _ tc, args) -> do
      argsWHNF <- traverse extractAndNormaliseArg args
      blockOnMetas tc argsWHNF $ case (tc, argsWHNF) of
          (IsContainer,    [t1, t2]) -> solveIsContainer    c t1 t2
          (HasEq,          [t1, t2]) -> solveHasEq          c t1 t2
          (HasOrd,         [t1, t2]) -> solveHasOrd         c t1 t2
          (IsTruth,        [t])      -> solveIsTruth        c t
          (IsNatural,      [t])      -> solveIsNatural      c t
          (IsIntegral,     [t])      -> solveIsIntegral     c t
          (IsRational,     [t])      -> solveIsRational     c t
          (IsReal,         [t])      -> solveIsReal         c t
          (IsQuantifiable, [t1, t2]) -> solveIsQuantifiable c t1 t2
          _                          -> developerError $ "Unknown type-class" <+> squotes (pretty tc) <+> "args" <+> pretty argsWHNF
    _ -> developerError $ "Unknown type-class" <+> squotes (pretty eWHNF)

extractAndNormaliseArg :: MonadTCResolution m => CheckedArg -> m CheckedExpr
extractAndNormaliseArg (Arg _ Explicit e) = whnf e
extractAndNormaliseArg _ = developerError "Not expecting type-classes with non-explicit arguments"

blockOnMetas :: MonadTCResolution m => Builtin -> [CheckedExpr] -> m Progress -> m Progress
blockOnMetas tc args action = do
  let metas = filter isMeta (getNonInferableArgs tc args)
  logDebug $ pretty (getNonInferableArgs tc args)
  logDebug $ pretty metas
  if null metas
    then action
    else do
      logDebug $ "Blocked on non-inferable metas:" <+> pretty metas
      return Stuck

isMeta :: CheckedExpr -> Bool
isMeta e = case decomposeApp e of
  (Meta _ _, _) -> True
  _             -> False

-- TODO insert Bool classes

solveHasEq :: MonadTCResolution m
           => TypeClassConstraint
           -> CheckedExpr
           -> CheckedExpr
           -> m Progress
solveHasEq (m `Has` _) (Builtin _ Bool)  (Builtin _ Prop) = return $ solved m
solveHasEq (m `Has` _) (Builtin _ Prop)  (Builtin _ Prop) = return $ solved m
solveHasEq (m `Has` _) (Builtin _ Nat)   (Builtin _ Prop) = return $ solved m
solveHasEq (m `Has` _) (Builtin _ Int)   (Builtin _ Prop) = return $ solved m
solveHasEq (m `Has` _) (Builtin _ Real)  (Builtin _ Prop) = return $ solved m
solveHasEq constraint _ _                       =
  throwError $ TypeClassResolutionFailure constraint

solveHasOrd :: MonadTCResolution m
            => TypeClassConstraint
            -> CheckedExpr
            -> CheckedExpr
            -> m Progress
solveHasOrd (m `Has` _) (Builtin _ Nat)   (Builtin _ Prop) = return $ solved m
solveHasOrd (m `Has` _) (Builtin _ Int)   (Builtin _ Prop) = return $ solved m
solveHasOrd (m `Has` _) (Builtin _ Real)  (Builtin _ Prop) = return $ solved m
solveHasOrd constraint  _                 _                =
  throwError $ TypeClassResolutionFailure constraint

solveIsTruth :: MonadTCResolution m
             => TypeClassConstraint
             -> CheckedExpr
             -> m Progress
solveIsTruth (m `Has` _) (Builtin _ Bool) = return $ solved m
solveIsTruth (m `Has` _) (Builtin _ Prop) = return $ solved m
solveIsTruth constraint  _                =
  throwError $ TypeClassResolutionFailure constraint

solveIsContainer :: MonadTCResolution m
                 => TypeClassConstraint
                 -> CheckedExpr
                 -> CheckedExpr
                 -> m Progress
solveIsContainer constraint@(m `Has` _) tCont tElem = do
  tContElem <- getContainerElem tCont
  case tContElem of
    Nothing -> return Stuck
    Just t  -> if t == tElem
      then return $ solved m
      else do
        addUnificationConstraint $ makeConstraint (prov constraint) [] tElem t
        return $ PartiallySolved mempty
  where
    getContainerElem :: MonadTCResolution m => CheckedExpr -> m (Maybe CheckedExpr)
    getContainerElem t = case decomposeApp t of
      (Builtin _ List,   [tElem'])    -> whnf (argExpr tElem') <&> Just
      (Builtin _ Tensor, [tElem', _]) -> whnf (argExpr tElem') <&> Just
      _                               -> return Nothing

solveIsNatural :: MonadTCResolution m
             => TypeClassConstraint
             -> CheckedExpr
             -> m Progress
solveIsNatural (m `Has` _) (Builtin _ Nat)  = return $ solved m
solveIsNatural (m `Has` _) (Builtin _ Int)  = return $ solved m
solveIsNatural (m `Has` _) (Builtin _ Real) = return $ solved m
solveIsNatural constraint _       =
  throwError $ TypeClassResolutionFailure constraint

solveIsIntegral :: MonadTCResolution m
             => TypeClassConstraint
             -> CheckedExpr
             -> m Progress
solveIsIntegral (m `Has` _) (Builtin _ Int)  = return $ solved m
solveIsIntegral (m `Has` _) (Builtin _ Real) = return $ solved m
solveIsIntegral constraint _                 =
  throwError $ TypeClassResolutionFailure constraint

solveIsRational :: MonadTCResolution m
             => TypeClassConstraint
             -> CheckedExpr
             -> m Progress
solveIsRational (m `Has` _) (Builtin _ Real) = return $ solved m
solveIsRational constraint _                 =
  throwError $ TypeClassResolutionFailure constraint

solveIsReal :: MonadTCResolution m
             => TypeClassConstraint
             -> CheckedExpr
             -> m Progress
solveIsReal (m `Has` _) (Builtin _ Real) = return $ solved m
solveIsReal constraint _                 =
  throwError $ TypeClassResolutionFailure constraint

-- TODO
solveIsQuantifiable :: MonadTCResolution m
                    => TypeClassConstraint
                    -> CheckedExpr
                    -> CheckedExpr
                    -> m Progress
solveIsQuantifiable constraint _ _ =
  throwError $ TypeClassResolutionFailure constraint