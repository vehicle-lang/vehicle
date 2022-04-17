
module Vehicle.Compile.Type
  ( typeCheck
  ) where

import Prelude hiding (pi)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (evalStateT)
import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)
import Data.IntSet qualified as IntSet

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.Print
import Vehicle.Compile.Type.Unify
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.TypeClass
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.MetaSubstitution (metasIn)

-------------------------------------------------------------------------------
-- Algorithm

typeCheck :: ( MonadCompile m
             , Inferrable a b
             , PrettyWith ('As 'Internal) b
             , MetaSubstitutable b )
          => a -> m b
typeCheck e = do
  let prog1 = runAll e
  let prog2 = runReaderT prog1 emptyVariableCtx
  prog3 <- evalStateT prog2 emptyMetaCtx
  return prog3

runAll :: ( TCM m
          , Inferrable a b
          , MetaSubstitutable b
          , PrettyWith ('As 'Internal) b)
       => a -> m b
runAll expr = do
  inferredExpr     <- infer expr
  metaSubstitution <- solveMetas
  let finalExpr = substMetas metaSubstitution inferredExpr
  logDebug $ prettyVerbose finalExpr <> line
  return finalExpr

solveMetas :: MonadConstraintSolving m => m MetaSubstitution
solveMetas = do
  logDebug "Starting constraint solving\n"
  constraints <- getConstraints
  solution <- loopOverConstraints $ Progress
    { newConstraints = constraints
    , solvedMetas    = mempty
    }
  logDebug "Finished constraint solving\n"
  return solution

loopOverConstraints :: MonadConstraintSolving m
                    => ConstraintProgress
                    -> m MetaSubstitution
loopOverConstraints progress = do
  constraints <- getConstraints
  currentSubstitution <- getMetaSubstitution
  case constraints of
    -- If there are no outstanding constraints then check that
    -- all metas have been solved, and if so return the solution
    [] -> do
      let metasSolved = metasIn currentSubstitution
      metasCreated <- (\v -> IntSet.fromList [0..v-1]) <$> numberOfMetasCreated
      let unsolvedMetas = IntSet.difference metasCreated metasSolved
      case IntSet.toList unsolvedMetas of
        []     -> return currentSubstitution
        m : ms -> do
          metasAndOrigins <- forM (m :| ms) (\v -> do
            let meta = MetaVar v
            origin <- getMetaOrigin meta
            return (meta, origin))
          throwError $ UnsolvedMetas metasAndOrigins

    -- Otherwise see if we made progress last iteration
    (c : cs) -> case progress of
      Progress _newConstraints _solvedMetas -> do
        -- If we have made useful progress then start a new pass

        -- TODO try to solve only either new constraints or those that contain
        -- blocking metas that were solved last iteration.
        let updatedConstraints = substMetas currentSubstitution constraints
        newProgress <- solveConstraints updatedConstraints
        loopOverConstraints newProgress

      Stuck -> do
        -- If we're stuck then try to solve type class constraints using
        -- default values.
        progressOnDefaults <- addNewConstraintsUsingDefaults constraints
        case progressOnDefaults of
          -- If we're still stuck then time to abort
          Stuck -> throwError $ UnsolvedConstraints (c :| cs)
          -- Otherwise start over with the remaining constraints
          _     -> loopOverConstraints progressOnDefaults

-------------------------------------------------------------------------------
-- Standard constraint solving

-- | Deterministic pass
solveConstraints :: MonadConstraintSolving m
                 => [Constraint]
                 -> m ConstraintProgress
solveConstraints constraints = do
  logDebug "Starting new pass"
  logDebug $ "current-constraints:" <+> align (prettyVerbose constraints)

  setConstraints []
  newProgress <- mconcat `fmap` traverse solveConstraint constraints

  metaSubst <- getMetaSubstitution
  logDebug $ "current-solution:" <+> prettyVerbose metaSubst <> "\n"
  return newProgress

-- | Tries to solve a constraint deterministically.
solveConstraint :: MonadConstraintSolving m
                => Constraint
                -> m ConstraintProgress
solveConstraint constraint = do
  logDebug $ "trying" <+> prettyVerbose constraint
  incrCallDepth

  result <- case constraint of
    UC ctx c -> solveUnificationConstraint ctx c
    TC ctx c -> solveTypeClassConstraint   ctx c

  case result of
    Progress newConstraints _ -> addConstraints newConstraints
    Stuck                     -> addConstraints [constraint]

  decrCallDepth
  return result

-------------------------------------------------------------------------------
-- Default constraint solving

-- |Tries to add new unification constraints using default values.
addNewConstraintsUsingDefaults :: MonadConstraintSolving m
                               => [Constraint]
                               -> m ConstraintProgress
addNewConstraintsUsingDefaults constraints = do
  logDebug "Temporarily stuck"
  logDebug "Trying default type-class constraints"
  incrCallDepth
  let tcConstraints = mapMaybe getTypeClassConstraint constraints

  result <- solveDefaultTypeClassConstraints tcConstraints
  case result of
    Progress newConstraints _ -> addConstraints newConstraints
    Stuck                     -> return ()

  logDebug "\n"
  decrCallDepth
  return result
  where
    getTypeClassConstraint :: Constraint -> Maybe (TypeClassConstraint, ConstraintContext)
    getTypeClassConstraint (TC ctx c) = Just (c, ctx)
    getTypeClassConstraint _          = Nothing