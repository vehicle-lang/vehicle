module Vehicle.Compile.Type.Subsystem.Standard.Constraint.TypeClassDefaults
  ( addNewConstraintUsingDefaults,
  )
where

import Control.Monad (filterM, foldM)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (evalApp, runEmptyNormT)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Substitution
import Vehicle.Compile.Type.Meta.Variable
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalised

-- | Tries to add new unification constraints using default values.
addNewConstraintUsingDefaults ::
  (TCM StandardBuiltinType m) =>
  Maybe StandardDecl ->
  m Bool
addNewConstraintUsingDefaults maybeDecl = do
  logDebug MaxDetail $ "Temporarily stuck" <> line

  logCompilerPass
    MidDetail
    "trying to generate a new constraint using type-classes defaults"
    $ do
      -- Calculate the set of candidate constraints
      candidateConstraints <- getDefaultCandidates maybeDecl
      logDebug MaxDetail $
        "Candidate type-class constraints:"
          <> line
          <> indent 2 (prettyVerbose candidateConstraints)
          <> line

      result <- generateConstraintUsingDefaults candidateConstraints
      case result of
        Nothing -> return False
        Just newConstraint -> do
          addConstraints [newConstraint]
          return True

getDefaultCandidates ::
  forall m.
  (TCM StandardBuiltinType m) =>
  Maybe StandardDecl ->
  m [WithContext StandardTypeClassConstraint]
getDefaultCandidates maybeDecl = do
  typeClassConstraints <- getActiveTypeClassConstraints
  case maybeDecl of
    Nothing -> return typeClassConstraints
    Just decl -> do
      declType <- substMetas (typeOf decl)

      -- We only want to generate default solutions for constraints
      -- that *don't* appear in the type of the declaration, as those will be
      -- quantified over later.
      constraints <- getActiveConstraints
      typeMetas <- getMetasLinkedToMetasIn constraints declType

      whenM (loggingLevelAtLeast MaxDetail) $ do
        unsolvedMetasInTypeDoc <- prettyMetas (Proxy @StandardBuiltinType) typeMetas
        logDebug MaxDetail $
          "Metas transitively related to type-signature:" <+> unsolvedMetasInTypeDoc

      flip filterM typeClassConstraints $ \tc -> do
        constraintMetas <- metasIn (objectIn tc)
        return $ MetaSet.disjoint constraintMetas typeMetas

--------------------------------------------------------------------------------
-- Default solutions to type-class constraints

-- This is some pretty ugly code. There must be a way of making this process
-- more elegant....

data NumericType
  = NatT
  | IntT
  | RatT
  deriving (Eq, Ord)

data DefaultFamily
  = NumericFamily NumericType Bool Int
  | ContainerFamily Bool
  | BooleanFamily
  | PolarityFamily
  | LinearityFamily
  deriving (Eq, Ord)

sameFamily :: DefaultFamily -> DefaultFamily -> Bool
sameFamily NumericFamily {} NumericFamily {} = True
sameFamily ContainerFamily {} ContainerFamily {} = True
sameFamily BooleanFamily {} BooleanFamily {} = True
sameFamily PolarityFamily {} PolarityFamily {} = True
sameFamily LinearityFamily {} LinearityFamily {} = True
sameFamily _ _ = False

data Candidate = Candidate
  { candidateTypeClass :: TypeClass,
    candidateMetaExpr :: StandardNormExpr,
    candidateCtx :: StandardConstraintContext,
    candidateSolution :: StandardNormExpr
  }

instance Pretty Candidate where
  pretty Candidate {..} =
    prettyVerbose candidateMetaExpr <+> "~" <+> pretty candidateTypeClass

data CandidateStatus
  = Valid Candidate
  | None
  | Invalid

instance Pretty CandidateStatus where
  pretty = \case
    Valid c -> pretty c
    None -> "none encountered"
    Invalid -> "incompatible"

generateConstraintUsingDefaults ::
  forall m.
  (TCM StandardBuiltinType m) =>
  [WithContext StandardTypeClassConstraint] ->
  m (Maybe (WithContext StandardConstraint))
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
          <+> "         " <> parens ("from" <+> pretty candidateTypeClass)
      let unificationConstraint = UnificationConstraint (Unify candidateMetaExpr candidateSolution)
      newConstraint <- WithContext unificationConstraint <$> copyContext candidateCtx
      return $ Just newConstraint

findStrongestConstraint ::
  (MonadCompile m) =>
  [WithContext StandardTypeClassConstraint] ->
  m CandidateStatus
findStrongestConstraint [] = return None
findStrongestConstraint (c@(WithContext constraint ctx) : cs) = do
  recResult <- findStrongestConstraint cs

  logCompilerSection MaxDetail ("considering" <+> squotes (prettyVerbose c)) $ do
    candidates <- getCandidatesFromConstraint ctx constraint

    newStrongest <- foldM strongest recResult candidates
    logDebug MaxDetail $ indent 2 $ "status:" <+> pretty newStrongest
    return newStrongest

strongest :: (MonadCompile m) => CandidateStatus -> Candidate -> m CandidateStatus
strongest Invalid _ = return Invalid
strongest None x = return $ Valid x
strongest y@(Valid c1) c2 = do
  f1 <- familyOf (candidateTypeClass c1)
  f2 <- familyOf (candidateTypeClass c2)
  return $
    if not (sameFamily f1 f2)
      then y
      else
        if f1 > f2
          then Valid c2
          else y

familyOf :: (MonadCompile m) => TypeClass -> m DefaultFamily
familyOf = \case
  HasQuantifier {} -> return BooleanFamily
  HasEq {} -> return $ NumericFamily NatT False 0
  HasOrd {} -> return $ NumericFamily NatT False 0
  HasAdd -> return $ NumericFamily NatT True 0
  HasSub -> return $ NumericFamily IntT True 0
  HasMul -> return $ NumericFamily NatT True 0
  HasDiv -> return $ NumericFamily RatT True 0
  HasNeg -> return $ NumericFamily IntT True 0
  HasNatLits -> return $ NumericFamily NatT False 0
  HasRatLits -> return $ NumericFamily RatT False 0
  HasVecLits {} -> return $ ContainerFamily True
  HasMap -> return $ ContainerFamily False
  HasFold -> return $ ContainerFamily False
  HasQuantifierIn {} -> return $ ContainerFamily False
  NatInDomainConstraint -> return $ NumericFamily NatT False 0

getCandidatesFromConstraint ::
  forall m.
  (MonadCompile m) =>
  StandardConstraintContext ->
  StandardTypeClassConstraint ->
  m [Candidate]
getCandidatesFromConstraint ctx (Has _ b args) = case b of
  StandardTypeClass tc -> do
    let getCandidates = getCandidatesFromArgs ctx
    case (tc, args) of
      (HasOrd ord, [tArg1, tArg2, _tRes]) -> return $ getCandidates (HasOrd ord) VNatType [tArg1, tArg2]
      (HasNeg, [tArg, _tRes]) -> return $ getCandidates HasNeg VIntType [tArg]
      (HasMul, [tArg1, tArg2, _tRes]) -> return $ getCandidates HasMul VNatType [tArg1, tArg2]
      (HasDiv, [tArg1, tArg2, _tRes]) -> return $ getCandidates HasDiv VRatType [tArg1, tArg2]
      (HasNatLits, [t]) -> return $ getCandidates HasNatLits VNatType [t]
      (HasRatLits, [t]) -> return $ getCandidates HasRatLits VRatType [t]
      (HasVecLits, [_n, t]) -> return $ getCandidates HasVecLits VRawListType [t]
      (HasMap, [t]) -> return $ getCandidates HasMap VRawListType [t]
      (HasFold, [t]) -> return $ getCandidates HasFold VRawListType [t]
      (NatInDomainConstraint, [n, t]) -> case t of
        VIndexType size -> do
          succN <- runEmptyNormT @StandardBuiltinType @m $ evalApp (VBuiltinFunction (Add AddNat) []) [ExplicitArg mempty n, ExplicitArg mempty (VNatLiteral 1)]
          return $ getCandidates NatInDomainConstraint succN [size]
        _ -> return []
      _ -> return []
  _ -> return []

getCandidatesFromArgs ::
  StandardConstraintContext ->
  TypeClass ->
  StandardNormExpr ->
  [StandardNormExpr] ->
  [Candidate]
getCandidatesFromArgs ctx tc solution ts = map mkCandidate (filter isNMeta ts)
  where
    mkCandidate t =
      Candidate
        { candidateTypeClass = tc,
          candidateMetaExpr = t,
          candidateCtx = ctx,
          candidateSolution = solution
        }
