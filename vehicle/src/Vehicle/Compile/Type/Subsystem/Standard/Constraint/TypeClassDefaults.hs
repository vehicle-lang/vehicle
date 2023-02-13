module Vehicle.Compile.Type.Subsystem.Standard.Constraint.TypeClassDefaults
  ( addNewConstraintUsingDefaults,
  )
where

import Control.Monad (filterM, foldM)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error
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
  TCM StandardBuiltinType m =>
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
  TCM StandardBuiltinType m =>
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

data Candidate = Candidate MetaID TypeClass StandardNormExpr StandardConstraintContext

instance Pretty Candidate where
  pretty (Candidate m tc _ _) = pretty m <+> "~" <+> pretty tc

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
  TCM StandardBuiltinType m =>
  [WithContext StandardTypeClassConstraint] ->
  m (Maybe (WithContext StandardConstraint))
generateConstraintUsingDefaults constraints = do
  strongestConstraint <- findStrongestConstraint constraints
  case strongestConstraint of
    None -> do
      logDebug MaxDetail "No default solution found"
      return Nothing
    Invalid -> return Nothing
    Valid (Candidate m tc metaExpr ctx) -> do
      solution <- defaultSolution tc
      logDebug MaxDetail $
        "using default"
          <+> pretty m
          <+> "="
          <+> prettyVerbose solution
          <+> "         " <> parens ("from" <+> pretty tc)
      let unificationConstraint = UnificationConstraint (Unify metaExpr solution)
      newConstraint <- WithContext unificationConstraint <$> copyContext ctx
      return $ Just newConstraint

findStrongestConstraint ::
  MonadCompile m =>
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

strongest :: MonadCompile m => CandidateStatus -> Candidate -> m CandidateStatus
strongest Invalid _ = return Invalid
strongest None x = return $ Valid x
strongest y@(Valid (Candidate _ tc2 _ _)) x@(Candidate _ tc1 _ _) = do
  f1 <- familyOf tc1
  f2 <- familyOf tc2
  return $
    if not (sameFamily f1 f2)
      then y
      else
        if f1 > f2
          then Valid x
          else y

familyOf :: MonadCompile m => TypeClass -> m DefaultFamily
familyOf = \case
  HasQuantifier {} -> return BooleanFamily
  HasEq {} -> return $ NumericFamily NatT False 0
  HasOrd {} -> return $ NumericFamily NatT False 0
  HasAdd -> return $ NumericFamily NatT True 0
  HasSub -> return $ NumericFamily IntT True 0
  HasMul -> return $ NumericFamily NatT True 0
  HasDiv -> return $ NumericFamily RatT True 0
  HasNeg -> return $ NumericFamily IntT True 0
  (HasNatLits n) -> return $ NumericFamily NatT False n
  HasRatLits -> return $ NumericFamily RatT False 0
  HasVecLits {} -> return $ ContainerFamily True
  HasMap -> return $ ContainerFamily False
  HasFold -> return $ ContainerFamily False
  HasQuantifierIn {} -> return $ ContainerFamily False
  NatInDomainConstraint n -> return $ NumericFamily NatT False n

defaultSolution ::
  TCM StandardBuiltinType m =>
  TypeClass ->
  m StandardNormExpr
defaultSolution = \case
  HasEq {} -> return VNatType
  HasOrd {} -> return VNatType
  HasQuantifier {} -> return VBoolType
  HasAdd -> return VNatType
  HasSub -> return VIntType
  HasMul -> return VNatType
  HasDiv -> return VRatType
  HasNeg -> return VIntType
  HasNatLits n -> return $ VIndexType (VNatLiteral (n + 1))
  HasRatLits -> return VRatType
  HasVecLits {} -> createDefaultListType
  HasMap -> createDefaultListType
  HasFold -> createDefaultListType
  HasQuantifierIn {} -> createDefaultListType
  NatInDomainConstraint n -> return $ VNatLiteral (n + 1)

createDefaultListType ::
  TCM StandardBuiltinType m =>
  m StandardNormType
createDefaultListType = return $ VBuiltinType List mempty

getCandidatesFromConstraint ::
  MonadCompile m =>
  StandardConstraintContext ->
  StandardTypeClassConstraint ->
  m [Candidate]
getCandidatesFromConstraint ctx (Has _ b args) = case b of
  StandardTypeClass tc -> do
    let getCandidate = getCandidatesFromArgs ctx
    return $ case (tc, args) of
      (HasOrd ord, [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] (HasOrd ord)
      (HasNeg, [tArg, _tRes]) -> getCandidate [tArg] HasNeg
      (HasMul, [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] HasMul
      (HasDiv, [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] HasDiv
      (HasNatLits n, [t]) -> getCandidate [t] (HasNatLits n)
      (HasRatLits, [t]) -> getCandidate [t] HasRatLits
      (HasVecLits, [_n, t]) -> getCandidate [t] HasVecLits
      (HasMap, [t]) -> getCandidate [t] HasMap
      (HasFold, [t]) -> getCandidate [t] HasFold
      (NatInDomainConstraint n, [t]) -> case t of
        VIndexType size -> getCandidate [size] (NatInDomainConstraint n)
        _ -> []
      _ -> []
  _ -> return []

getCandidatesFromArgs :: StandardConstraintContext -> [StandardNormExpr] -> TypeClass -> [Candidate]
getCandidatesFromArgs ctx ts tc = catMaybes $ flip map ts $ \t -> do
  case getNMeta t of
    Just m -> Just (Candidate m tc t ctx) -- m, t, tc)
    _ -> Nothing
