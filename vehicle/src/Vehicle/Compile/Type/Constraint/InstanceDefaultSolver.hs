module Vehicle.Compile.Type.Constraint.InstanceDefaultSolver
  ( HasInstanceDefaults (..),
    addNewConstraintUsingDefaults,
    Candidate (..),
  )
where

import Control.Monad (filterM, foldM)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Constraint.Core (createInstanceUnification)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Variable
import Vehicle.Compile.Type.Monad
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Expr.Value

class HasInstanceDefaults builtin where
  getCandidatesFromConstraint ::
    forall m.
    (MonadCompile m) =>
    ConstraintContext builtin ->
    InstanceConstraint builtin ->
    m [Candidate builtin]

  compareCandidates :: Candidate builtin -> Candidate builtin -> Maybe Ordering

type MonadInstanceDefault builtin m =
  ( TCM builtin m,
    HasInstanceDefaults builtin
  )

-- | Tries to add new unification constraints using default values.
addNewConstraintUsingDefaults ::
  (TCM builtin m, HasInstanceDefaults builtin) =>
  Maybe (Decl Ix builtin) ->
  m Bool
addNewConstraintUsingDefaults maybeDecl = do
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

      result <- generateConstraintUsingDefaults defaultableConstraints
      case result of
        Nothing -> return False
        Just newConstraint -> do
          addConstraints [newConstraint]
          return True

getDefaultableConstraints ::
  forall builtin m.
  (MonadInstanceDefault builtin m) =>
  Maybe (Decl Ix builtin) ->
  m [WithContext (InstanceConstraint builtin)]
getDefaultableConstraints maybeDecl = do
  typeClassConstraints <- getActiveInstanceConstraints
  case maybeDecl of
    Nothing -> return typeClassConstraints
    Just decl -> do
      declType <- substMetas (typeOf decl)

      -- We only want to generate default solutions for constraints
      -- that *don't* appear in the type of the declaration, as those will be
      -- quantified over later.
      constraints <- getActiveConstraints
      typeMetas <- getMetasLinkedToMetasIn constraints declType

      logDebugM MaxDetail $ do
        unsolvedMetasInTypeDoc <- prettyMetas (Proxy @builtin) typeMetas
        return $ "Metas transitively related to type-signature:" <+> unsolvedMetasInTypeDoc

      flip filterM typeClassConstraints $ \tc -> do
        constraintMetas <- metasIn (objectIn tc)
        return $ MetaSet.disjoint constraintMetas typeMetas

--------------------------------------------------------------------------------
-- Default solutions to type-class constraints

data Candidate builtin = Candidate
  { candidateTypeClass :: builtin,
    candidateMetaExpr :: WHNFValue builtin,
    candidateInfo :: InstanceConstraintInfo builtin,
    candidateSolution :: WHNFValue builtin
  }

instance (PrintableBuiltin builtin) => Pretty (Candidate builtin) where
  pretty Candidate {..} =
    prettyVerbose candidateMetaExpr <+> "~" <+> prettyVerbose (VBuiltin @(WHNFClosure builtin) @builtin candidateTypeClass [])

data CandidateStatus builtin
  = Valid (Candidate builtin)
  | None
  | Invalid

instance (PrintableBuiltin builtin) => Pretty (CandidateStatus builtin) where
  pretty = \case
    Valid c -> pretty c
    None -> "none encountered"
    Invalid -> "incompatible"

generateConstraintUsingDefaults ::
  forall builtin m.
  (MonadInstanceDefault builtin m) =>
  [WithContext (InstanceConstraint builtin)] ->
  m (Maybe (WithContext (Constraint builtin)))
generateConstraintUsingDefaults constraints = do
  strongestConstraint <- findStrongestConstraint constraints
  case strongestConstraint of
    None -> do
      logDebug MaxDetail "No default solution found"
      return Nothing
    Invalid -> return Nothing
    Valid Candidate {..} -> do
      logDebug MaxDetail $
        "using default"
          <+> prettyVerbose candidateMetaExpr
          <+> "="
          <+> prettyVerbose candidateSolution
          <+> "         "
          <> parens ("from" <+> prettyVerbose (VBuiltin @(WHNFClosure builtin) @builtin candidateTypeClass []))
      newConstraint <- createInstanceUnification candidateInfo candidateMetaExpr candidateSolution
      return $ Just newConstraint

findStrongestConstraint ::
  (MonadInstanceDefault builtin m) =>
  [WithContext (InstanceConstraint builtin)] ->
  m (CandidateStatus builtin)
findStrongestConstraint [] = return None
findStrongestConstraint (c@(WithContext constraint ctx) : cs) = do
  recResult <- findStrongestConstraint cs

  logCompilerSection MaxDetail ("considering" <+> squotes (prettyVerbose c)) $ do
    candidates <- getCandidatesFromConstraint ctx constraint

    newStrongest <- foldM strongest recResult candidates
    logDebug MaxDetail $ indent 2 $ "status:" <+> pretty newStrongest
    return newStrongest

strongest ::
  (MonadInstanceDefault builtin m) =>
  CandidateStatus builtin ->
  Candidate builtin ->
  m (CandidateStatus builtin)
strongest Invalid _ = return Invalid
strongest None x = return $ Valid x
strongest y@(Valid c1) c2 = do
  case compareCandidates c1 c2 of
    Just GT -> return $ Valid c2
    _ -> return y
