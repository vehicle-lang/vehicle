module Vehicle.Compile.Type.Subsystem.Standard.Constraint.TypeClassDefaults
  ( addNewConstraintUsingDefaults,
  )
where

import Control.Monad (filterM, foldM)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Constraint.Core (parseInstanceGoal)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Substitution
import Vehicle.Compile.Type.Meta.Variable
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Expr.Normalised (Value (..), isNMeta)

-- | Tries to add new unification constraints using default values.
addNewConstraintUsingDefaults ::
  (TCM StandardBuiltin m) =>
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
  (TCM StandardBuiltin m) =>
  Maybe StandardDecl ->
  m [WithContext StandardInstanceConstraint]
getDefaultCandidates maybeDecl = do
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
        unsolvedMetasInTypeDoc <- prettyMetas (Proxy @StandardBuiltin) typeMetas
        return $ "Metas transitively related to type-signature:" <+> unsolvedMetasInTypeDoc

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
  { candidateTypeClass :: StandardBuiltin,
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
  (TCM StandardBuiltin m) =>
  [WithContext StandardInstanceConstraint] ->
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
          <+> "         "
          <> parens ("from" <+> pretty candidateTypeClass)
      let unificationConstraint = UnificationConstraint (Unify candidateMetaExpr candidateSolution)
      newConstraint <- WithContext unificationConstraint <$> copyContext candidateCtx
      return $ Just newConstraint

findStrongestConstraint ::
  (MonadCompile m) =>
  [WithContext StandardInstanceConstraint] ->
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

familyOf :: (MonadCompile m) => StandardBuiltin -> m DefaultFamily
familyOf = \case
  TypeClass HasQuantifier {} -> return BooleanFamily
  TypeClass HasEq {} -> return $ NumericFamily NatT False 0
  TypeClass HasOrd {} -> return $ NumericFamily NatT False 0
  TypeClass HasAdd -> return $ NumericFamily NatT True 0
  TypeClass HasSub -> return $ NumericFamily IntT True 0
  TypeClass HasMul -> return $ NumericFamily NatT True 0
  TypeClass HasDiv -> return $ NumericFamily RatT True 0
  TypeClass HasNeg -> return $ NumericFamily IntT True 0
  TypeClass HasNatLits -> return $ NumericFamily NatT False 0
  TypeClass HasRatLits -> return $ NumericFamily RatT False 0
  TypeClass HasVecLits {} -> return $ ContainerFamily True
  TypeClass HasMap -> return $ ContainerFamily False
  TypeClass HasFold -> return $ ContainerFamily False
  TypeClass HasQuantifierIn {} -> return $ ContainerFamily False
  NatInDomainConstraint -> return $ NumericFamily NatT False 0
  _ -> compilerDeveloperError "Malformed instance constraint"

getCandidatesFromConstraint ::
  forall m.
  (MonadCompile m) =>
  StandardConstraintContext ->
  StandardInstanceConstraint ->
  m [Candidate]
getCandidatesFromConstraint ctx (Has _ _ expr) = do
  InstanceGoal {..} <- parseInstanceGoal expr
  let getCandidates = getCandidatesFromArgs ctx
  let defaults = case (goalHead, goalSpine) of
        (TypeClass HasOrd {}, [tArg1, tArg2, _tRes]) -> Just (VNatType, [tArg1, tArg2])
        (TypeClass HasNeg, [tArg, _tRes]) -> Just (VIntType, [tArg])
        (TypeClass HasMul, [tArg1, tArg2, _tRes]) -> Just (VNatType, [tArg1, tArg2])
        (TypeClass HasDiv, [tArg1, tArg2, _tRes]) -> Just (VRatType, [tArg1, tArg2])
        (TypeClass HasNatLits, [t]) -> Just (VNatType, [t])
        (TypeClass HasRatLits, [t]) -> Just (VRatType, [t])
        (TypeClass HasVecLits, [_n, t]) -> Just (VRawListType, [t])
        (TypeClass HasMap, [t]) -> Just (VRawListType, [t])
        (TypeClass HasFold, [t]) -> Just (VRawListType, [t])
        (NatInDomainConstraint, [n, t]) -> case argExpr t of
          VIndexType size -> do
            succN <- do
              let maybeNormResult = evalAddNat [argExpr n, VNatLiteral 1]
              let defaultExpr = VBuiltin (BuiltinFunction (Add AddNat)) [n, RelevantExplicitArg mempty (VNatLiteral 1)]
              return $ fromMaybe defaultExpr maybeNormResult
            Just (succN, [IrrelevantImplicitArg mempty size])
          _ -> Nothing
        _ -> Nothing

  case defaults of
    Nothing -> return []
    Just (defaultValue, defaultArgs) -> return $ getCandidates goalHead defaultValue defaultArgs

getCandidatesFromArgs ::
  StandardConstraintContext ->
  StandardBuiltin ->
  StandardNormExpr ->
  StandardSpine ->
  [Candidate]
getCandidatesFromArgs ctx tc solution ts = map mkCandidate (filter (isNMeta . argExpr) ts)
  where
    mkCandidate t =
      Candidate
        { candidateTypeClass = tc,
          candidateMetaExpr = argExpr t,
          candidateCtx = ctx,
          candidateSolution = solution
        }
