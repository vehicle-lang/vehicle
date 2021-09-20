

module Vehicle.Core.Compile.Type.TypeClass
  ( TypeClassConstraint(..)
  , TypeClassResolutionError
  , solveTypeClassConstraints
  )
  where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.Writer (MonadWriter(..), runWriterT)
import Data.IntMap as IntMap ( singleton, union )
import Prettyprinter ( (<+>), Pretty(pretty) )

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Print.Core ()

--------------------------------------------------------------------------------
-- Constraint definition

data TypeClassConstraint = Meta `Has` CheckedExpr
  deriving (Show)

instance Pretty TypeClassConstraint where
  pretty (m `Has` e) = "?" <> pretty m <+> "~" <+> pretty e

instance MetaSubstitutable TypeClassConstraint where
  substM (m `Has` e) = (m `Has`) <$> substM e

instance HasProvenance TypeClassConstraint where
  prov (_m `Has` e) = prov e

--------------------------------------------------------------------------------
-- Error messages

-- I'm sure there will be some errors once everything is fleshed out
-- so not going to remove it just yet
data TypeClassResolutionError

instance MeaningfulError TypeClassResolutionError where
  -- TODO what is the absurd pattern match in Haskell?
  details _e = developerError "No TypeClassResolutionError exists"

--------------------------------------------------------------------------------
-- Solution

type MonadTypeClassResolution m =
  ( MonadError TypeClassResolutionError m
  , MonadLogger m
  , MonadWriter [TypeClassConstraint] m -- Unsolved constraints
  )

-- | Tries to solve the provided list of constraints, returning the resulting
-- substitution for the solved constraints and a list of unsolved constraints if
-- any.
solveTypeClassConstraints :: [TypeClassConstraint] ->
                             ExceptT TypeClassResolutionError Logger
                              (MetaSubstitution, [TypeClassConstraint])
solveTypeClassConstraints constraints = do
  logDebug "Beginning type-class resolution"
  logDebug (layoutAsText $ "Type-class constraints" <+> pretty constraints)
  let result = runWriterT $ foldM (\t c -> IntMap.union t <$> solveConstraint c) mempty constraints
  logDebug (layoutAsText $ "Solution:" <+> pretty constraints)
  result

solveConstraint :: MonadTypeClassResolution m
                => TypeClassConstraint
                -> m MetaSubstitution
solveConstraint c@(m `Has` e)
  | hasDict e  = return $ IntMap.singleton m (PrimDict e)
  | otherwise = do tell [c]; return mempty

hasDict :: CheckedExpr -> Bool
hasDict e = case decomposeApp e of
  (Builtin _ HasEq,          [t1, t2]) -> solveHasEq t1 t2
  (Builtin _ HasOrd,         [t1, t2]) -> solveHasOrd t1 t2
  (Builtin _ IsTruth,        [t])      -> solveIsTruth t
  (Builtin _ IsContainer,    [t1, t2]) -> solveIsContainer t1 t2
  (Builtin _ IsNatural,      [t])      -> solveIsNatural t
  (Builtin _ IsIntegral,     [t])      -> solveIsIntegral t
  (Builtin _ IsRational,     [t])      -> solveIsRational t
  (Builtin _ IsReal,         [t])      -> solveIsReal t
  (Builtin _ IsQuantifiable, [t1, t2]) -> solveIsQuantifiable t1 t2
  _                                    -> False

-- TODO insert Bool classes

solveHasEq :: CheckedArg -> CheckedArg -> Bool
solveHasEq (Arg _ Explicit (Builtin _ Bool))  (Arg _ Explicit (Builtin _ Prop)) = True
solveHasEq (Arg _ Explicit (Builtin _ Prop))  (Arg _ Explicit (Builtin _ Prop)) = True
solveHasEq (Arg _ Explicit (Builtin _ Nat))   (Arg _ Explicit (Builtin _ Prop)) = True
solveHasEq (Arg _ Explicit (Builtin _ Int))   (Arg _ Explicit (Builtin _ Prop)) = True
solveHasEq (Arg _ Explicit (Builtin _ Real))  (Arg _ Explicit (Builtin _ Prop)) = True
solveHasEq _ _ = False

solveHasOrd :: CheckedArg -> CheckedArg -> Bool
solveHasOrd (Arg _ Explicit (Builtin _ Nat))   (Arg _ Explicit (Builtin _ Prop)) = True
solveHasOrd (Arg _ Explicit (Builtin _ Int))   (Arg _ Explicit (Builtin _ Prop)) = True
solveHasOrd (Arg _ Explicit (Builtin _ Real))  (Arg _ Explicit (Builtin _ Prop)) = True
solveHasOrd _ _ = False

solveIsTruth :: CheckedArg -> Bool
solveIsTruth (Arg _ Explicit (Builtin _ Bool)) = True
solveIsTruth (Arg _ Explicit (Builtin _ Prop)) = True
solveIsTruth _ = False

-- TODO
solveIsContainer :: CheckedArg -> CheckedArg -> Bool
solveIsContainer _ _ = False

solveIsNatural :: CheckedArg -> Bool
solveIsNatural (Arg _ Explicit (Builtin _ Nat))  = True
solveIsNatural (Arg _ Explicit (Builtin _ Int))  = True
solveIsNatural (Arg _ Explicit (Builtin _ Real)) = True
solveIsNatural _ = False

solveIsIntegral :: CheckedArg -> Bool
solveIsIntegral (Arg _ Explicit (Builtin _ Int))  = True
solveIsIntegral (Arg _ Explicit (Builtin _ Real)) = True
solveIsIntegral _ = False

solveIsRational :: CheckedArg -> Bool
solveIsRational (Arg _ Explicit (Builtin _ Real)) = True
solveIsRational _ = False

solveIsReal :: CheckedArg -> Bool
solveIsReal (Arg _ Explicit (Builtin _ Real)) = True
solveIsReal _ = False

-- TODO
solveIsQuantifiable :: CheckedArg -> CheckedArg -> Bool
solveIsQuantifiable _ _ = False