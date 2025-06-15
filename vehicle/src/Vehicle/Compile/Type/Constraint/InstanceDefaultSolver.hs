module Vehicle.Compile.Type.Constraint.InstanceDefaultSolver
  ( addNewInstanceConstraintUsingDefaults,
    getDefaultableConstraints,
  )
where

import Control.Monad (filterM)
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (In, NoCtx, PrettyVerbose, prettyVerbose)
import Vehicle.Compile.Type.Constraint.InstanceSolver (acceptCandidate)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Variable
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Interface.Print

type MonadInstanceDefault builtin m =
  ( MonadTypeChecker builtin m,
    Hashable builtin
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

addNewInstanceConstraintUsingDefaults ::
  forall builtin m.
  (MonadTypeChecker builtin m, Hashable builtin) =>
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
  (MonadInstanceDefault builtin m, HasMetas constraint, PrettyVerbose (Contextualised constraint ctx `In` NoCtx)) =>
  Proxy builtin ->
  [Contextualised constraint ctx] ->
  m [Contextualised constraint ctx]
getDefaultableConstraints proxy possibleConstraints = do
  maybeDecl <- getCurrentDeclAndUnused @builtin
  result <- case maybeDecl of
    Just (decl, declIsUnused) | not (isAbstractDecl decl || declIsUnused) -> do
      logDebug MaxDetail $ pretty declIsUnused
      -- We only want to generate default solutions for constraints
      -- that *don't* appear in the type of the declaration, as those will be
      -- quantified over later. However, if the declaration is unused then
      -- we don't care and we should use any defaults we can find.
      constraints <- getActiveConstraints
      typeMetas <- getMetasLinkedToMetasIn constraints (typeOf decl)

      logDebugM MaxDetail $ do
        unsolvedMetasInTypeDoc <- prettyMetas proxy typeMetas
        return $ "Metas transitively related to type-signature:" <+> line <> indent 2 unsolvedMetasInTypeDoc

      flip filterM possibleConstraints $ \tc -> do
        let constraintMetas = metasIn (objectIn tc)
        return $ MetaSet.disjoint constraintMetas typeMetas
    _ -> return possibleConstraints

  logDebug MaxDetail $
    "Suitable defaultable constraints:"
      <> line
      <> indent 2 (prettySetLike (fmap prettyVerbose result))
      <> line

  return result

chooseDefaultConstraint ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  [WithContext (InstanceConstraint builtin)] ->
  m (Maybe (DefaultCandidate builtin))
chooseDefaultConstraint constraints = do
  instanceDatabase <- getInstanceCandidates
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
  let goal = instanceGoal $ objectIn constraint
  case lookupDefaultInstance database goal of
    Just candidate -> Just $ DefaultCandidate (constraint, goal, candidate)
    Nothing -> Nothing

acceptDefaultCandidate ::
  forall builtin m.
  (MonadInstanceDefault builtin m) =>
  DefaultCandidate builtin ->
  m ()
acceptDefaultCandidate c@(DefaultCandidate (constraint, goal, candidate)) = do
  logDebug MaxDetail $ "using default" <+> pretty c
  _ <- removeInstanceConstraint (Proxy @builtin) (constraintID $ contextOf constraint)
  acceptCandidate constraint goal (WithContext candidate (boundContextOf $ contextOf constraint))
