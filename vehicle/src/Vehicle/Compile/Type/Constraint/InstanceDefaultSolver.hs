module Vehicle.Compile.Type.Constraint.InstanceDefaultSolver
  ( addNewInstanceConstraintUsingDefaults,
    getDefaultableConstraints,
  )
where

import Control.Monad (filterM)
import Data.Foldable (minimumBy)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrettyVerbose, prettyVerbose)
import Vehicle.Compile.Type.Constraint.InstanceSolver (acceptCandidate)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Variable
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Interface.Type (TypableBuiltin)

type MonadInstanceDefault builtin m =
  ( MonadTypeChecker builtin m,
    TypableBuiltin builtin
  )

newtype DefaultCandidate builtin
  = DefaultCandidate
      ( WithContext (InstanceConstraint builtin),
        InstanceGoal builtin,
        PossibleInstanceCandidate builtin
      )

instance (PrintableBuiltin builtin) => Pretty (DefaultCandidate builtin) where
  pretty (DefaultCandidate (constraint, _, candidate)) =
    prettyVerbose constraint <+> "~" <+> prettyVerbose (candidateExpr $ objectIn candidate)

addNewInstanceConstraintUsingDefaults ::
  forall builtin m.
  (MonadInstanceDefault builtin m) =>
  Proxy builtin ->
  m Bool
addNewInstanceConstraintUsingDefaults proxy = do
  instanceConstraints <- getActiveInstanceConstraints @builtin
  defaultableConstraints <- getDefaultableConstraints proxy instanceConstraints
  result <- chooseDefaultConstraint defaultableConstraints
  case result of
    Just candidate -> do
      acceptDefaultCandidate candidate
      return True
    Nothing -> return False

getDefaultableConstraints ::
  forall constraint ctx builtin m.
  (MonadInstanceDefault builtin m, HasMetas constraint, PrettyVerbose (Contextualised constraint ctx)) =>
  Proxy builtin ->
  [Contextualised constraint ctx] ->
  m [Contextualised constraint ctx]
getDefaultableConstraints proxy possibleConstraints = do
  maybeDecl <- getCurrentDeclAndUnused @builtin
  result <- case maybeDecl of
    Just (DefFunction _ _ _ t _, declIsUnused) | not declIsUnused -> do
      logDebug MidDetail $ pretty declIsUnused
      -- We only want to generate default solutions for constraints
      -- that *don't* appear in the type of the declaration, as those will be
      -- quantified over later. However, if the declaration is unused then
      -- we don't care and we should use any defaults we can find.
      constraints <- getActiveConstraints
      typeMetas <- getMetasLinkedToMetasIn constraints t

      logDebugM MidDetail $ do
        unsolvedMetasInTypeDoc <- prettyMetas proxy typeMetas
        return $ "Metas transitively related to type-signature:" <+> lineIndent unsolvedMetasInTypeDoc

      flip filterM possibleConstraints $ \tc -> do
        let constraintMetas = metasIn (objectIn tc)
        return $ MetaSet.disjoint constraintMetas typeMetas
    _ -> return possibleConstraints

  logDebug MidDetail $
    "Suitable defaultable constraints:"
      <> line
      <> indent 2 (prettySetLike (fmap prettyVerbose result))
      <> line

  return result

chooseDefaultConstraint ::
  forall builtin m.
  (MonadInstanceDefault builtin m) =>
  [WithContext (InstanceConstraint builtin)] ->
  m (Maybe (DefaultCandidate builtin))
chooseDefaultConstraint constraints = do
  defaults <- catMaybes <$> traverse findDefault constraints
  case defaults of
    [] -> do
      logDebug MidDetail "No default solution found"
      return Nothing
    candidate : _ -> do
      return $ Just candidate

findDefault ::
  (MonadInstanceDefault builtin m) =>
  WithContext (InstanceConstraint builtin) ->
  m (Maybe (DefaultCandidate builtin))
findDefault constraint = do
  let goal = instanceGoal $ objectIn constraint
  case instanceCandidateState $ objectIn constraint of
    Nothing -> developerError "No attempt has been made to solve constraint that we are trying to solve with defaults"
    Just (allCandidates, _) -> do
      let candidateAndPriorities = mapMaybe (\c -> (,c) <$> candidatePriority (objectIn c)) allCandidates
      return $ case candidateAndPriorities of
        [] -> Nothing
        cs -> do
          let highestPriorityCandidate = snd $ minimumBy (comparing fst) cs
          Just $ DefaultCandidate (constraint, goal, highestPriorityCandidate)

acceptDefaultCandidate ::
  forall builtin m.
  (MonadInstanceDefault builtin m) =>
  DefaultCandidate builtin ->
  m ()
acceptDefaultCandidate c@(DefaultCandidate (constraint, goal, candidate)) = do
  logDebug MidDetail $ "using default" <+> pretty c
  _ <- removeInstanceConstraint (Proxy @builtin) (constraintID $ contextOf constraint)
  _ <- acceptCandidate constraint goal candidate
  return ()
