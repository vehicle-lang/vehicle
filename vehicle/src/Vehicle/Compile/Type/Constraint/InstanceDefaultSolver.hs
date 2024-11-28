module Vehicle.Compile.Type.Constraint.InstanceDefaultSolver
  ( addNewConstraintUsingDefaults,
  )
where

import Control.Monad (filterM)
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrintableBuiltin, prettyVerbose)
import Vehicle.Compile.Type.Constraint.Core (parseInstanceGoal)
import Vehicle.Compile.Type.Constraint.InstanceSolver (acceptCandidate)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Variable
import Vehicle.Compile.Type.Monad.Class

type MonadInstanceDefault builtin m =
  ( MonadTypeChecker builtin m
  )

newtype DefaultCandidate builtin
  = DefaultCandidate
      ( WithContext (InstanceConstraint builtin),
        InstanceGoal builtin,
        InstanceCandidate builtin
      )

instance (PrintableBuiltin builtin) => Pretty (DefaultCandidate builtin) where
  pretty (DefaultCandidate (constraint, _, candidate)) =
    prettyVerbose constraint <+> "~" <+> prettyVerbose (candidateExpr candidate)

-- | Tries to add new unification constraints using default values.
addNewConstraintUsingDefaults ::
  (MonadTypeChecker builtin m, Hashable builtin) =>
  ([WithContext (InstanceConstraint builtin)] -> m Bool) ->
  InstanceDatabase builtin ->
  Maybe (Decl builtin) ->
  m Bool
addNewConstraintUsingDefaults handleNonStandardConstraints instanceDatabase maybeDecl = do
  logDebug MaxDetail $ "Temporarily stuck" <> line

  logCompilerPass
    MidDetail
    "trying to generate a new constraint using instance defaults"
    $ do
      -- Calculate the set of candidate constraints
      defaultableConstraints <- getDefaultableConstraints maybeDecl
      logDebug MaxDetail $
        "Suitable instance constraints:"
          <> line
          <> indent 2 (prettyVerbose defaultableConstraints)
          <> line

      result <- chooseDefaultConstraint instanceDatabase defaultableConstraints
      case result of
        Just candidate -> do
          acceptDefaultCandidate candidate
          return True
        Nothing ->
          handleNonStandardConstraints defaultableConstraints

getDefaultableConstraints ::
  forall builtin m.
  (MonadInstanceDefault builtin m) =>
  Maybe (Decl builtin) ->
  m [WithContext (InstanceConstraint builtin)]
getDefaultableConstraints maybeDecl = do
  instanceConstraints <- getActiveInstanceConstraints
  case maybeDecl of
    Just decl | not (isAbstractDecl decl) -> do
      -- We only want to generate default solutions for constraints
      -- that *don't* appear in the type of the declaration, as those will be
      -- quantified over later.
      declType <- substMetas (typeOf decl)

      constraints <- getActiveConstraints
      typeMetas <- getMetasLinkedToMetasIn constraints declType

      logDebugM MaxDetail $ do
        unsolvedMetasInTypeDoc <- prettyMetas (Proxy @builtin) typeMetas
        return $ "Metas transitively related to type-signature:" <+> unsolvedMetasInTypeDoc

      flip filterM instanceConstraints $ \tc -> do
        constraintMetas <- metasIn (objectIn tc)
        return $ MetaSet.disjoint constraintMetas typeMetas
    _ -> return instanceConstraints

chooseDefaultConstraint ::
  forall builtin m.
  (MonadCompile m, PrintableBuiltin builtin, Hashable builtin) =>
  InstanceDatabase builtin ->
  [WithContext (InstanceConstraint builtin)] ->
  m (Maybe (DefaultCandidate builtin))
chooseDefaultConstraint instanceDatabase constraints = do
  let defaults = mapMaybe (findDefault instanceDatabase) constraints
  case defaults of
    [] -> do
      logDebug MaxDetail "No default solution found"
      return Nothing
    candidate : _ -> do
      return $ Just candidate

findDefault ::
  (PrintableBuiltin builtin, Hashable builtin) =>
  InstanceDatabase builtin ->
  WithContext (InstanceConstraint builtin) ->
  Maybe (DefaultCandidate builtin)
findDefault database constraint = do
  let goal = parseInstanceGoal constraint
  case lookupDefaultInstance database goal of
    Just candidate -> Just $ DefaultCandidate (constraint, goal, candidate)
    Nothing -> Nothing

acceptDefaultCandidate ::
  (MonadInstanceDefault builtin m) =>
  DefaultCandidate builtin ->
  m ()
acceptDefaultCandidate c@(DefaultCandidate (constraint, goal, candidate)) = do
  logDebug MaxDetail $ "using default" <+> pretty c
  removeInstanceConstraint constraint
  acceptCandidate constraint goal (WithContext candidate (boundContextOf $ contextOf constraint))
