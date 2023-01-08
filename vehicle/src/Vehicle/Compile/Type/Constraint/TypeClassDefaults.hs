module Vehicle.Compile.Type.Constraint.TypeClassDefaults
  ( generateConstraintUsingDefaults,
  )
where

import Control.Monad (foldM)
import Data.Maybe (catMaybes)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Monad
import Vehicle.Expr.Normalised

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

data Candidate = Candidate MetaID TypeClass NormExpr ConstraintContext

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
  TCM m =>
  [WithContext TypeClassConstraint] ->
  m (Maybe (WithContext Constraint))
generateConstraintUsingDefaults constraints = do
  strongestConstraint <- findStrongestConstraint constraints
  case strongestConstraint of
    None -> do
      logDebug MaxDetail "No default solution found"
      return Nothing
    Invalid -> return Nothing
    Valid (Candidate m tc metaExpr ctx) -> do
      let p = inserted $ provenanceOf ctx
      let ctxSize = length (boundContext ctx)
      solution <- defaultSolution p ctxSize tc
      logDebug MaxDetail $
        "using default"
          <+> pretty m
          <+> "="
          <+> prettyVerbose solution
          <+> "         " <> parens ("from" <+> pretty tc)
      let unificationConstraint = UnificationConstraint (Unify metaExpr solution)
      let newConstraint = WithContext unificationConstraint (copyContext ctx)
      return $ Just newConstraint

findStrongestConstraint ::
  MonadCompile m =>
  [WithContext TypeClassConstraint] ->
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
  HasNot -> return BooleanFamily
  HasAnd -> return BooleanFamily
  HasOr -> return BooleanFamily
  HasImplies -> return BooleanFamily
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
  AlmostEqualConstraint {} -> auxiliaryTCError
  HasIf {} -> ifTCError
  LinearityTypeClass {} -> auxiliaryTCError
  PolarityTypeClass {} -> auxiliaryTCError

defaultSolution ::
  TCM m =>
  Provenance ->
  Int ->
  TypeClass ->
  m NormExpr
defaultSolution p ctxSize = \case
  HasEq {} -> return $ VNatType p
  HasOrd {} -> return $ VNatType p
  HasNot -> createDefaultBoolType p
  HasAnd -> createDefaultBoolType p
  HasOr -> createDefaultBoolType p
  HasImplies -> createDefaultBoolType p
  HasQuantifier {} -> createDefaultBoolType p
  HasAdd -> return $ VNatType p
  HasSub -> return $ VIntType p
  HasMul -> return $ VNatType p
  HasDiv -> createDefaultRatType p
  HasNeg -> return $ VIntType p
  HasNatLits n -> return $ VIndexType p (VNatLiteral p (n + 1))
  HasRatLits -> createDefaultRatType p
  HasVecLits {} -> createDefaultListType p ctxSize
  HasMap -> createDefaultListType p ctxSize
  HasFold -> createDefaultListType p ctxSize
  HasQuantifierIn {} -> createDefaultListType p ctxSize
  NatInDomainConstraint n -> return $ VNatLiteral p (n + 1)
  HasIf {} -> ifTCError
  LinearityTypeClass {} -> auxiliaryTCError
  PolarityTypeClass {} -> auxiliaryTCError
  AlmostEqualConstraint {} -> auxiliaryTCError

createDefaultListType :: TCM m => Provenance -> Int -> m NormType
createDefaultListType p ctxSize = do
  tElem <- normalised <$> freshExprMeta p (TypeUniverse p 0) ctxSize
  return $ VListType p tElem

createDefaultBoolType :: TCM m => Provenance -> m NormType
createDefaultBoolType p = do
  lin <- normalised <$> freshLinearityMeta p
  pol <- normalised <$> freshPolarityMeta p
  return $ VAnnBoolType p lin pol

createDefaultRatType :: TCM m => Provenance -> m NormType
createDefaultRatType p = do
  lin <- normalised <$> freshLinearityMeta p
  return $ VAnnRatType p lin

getCandidatesFromConstraint :: MonadCompile m => ConstraintContext -> TypeClassConstraint -> m [Candidate]
getCandidatesFromConstraint ctx (Has _ tc args) = do
  let getCandidate = getCandidatesFromArgs ctx
  return $ case (tc, args) of
    (HasEq eq, [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] (HasEq eq)
    (HasOrd ord, [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] (HasOrd ord)
    (HasNeg, [tArg, _tRes]) -> getCandidate [tArg] HasNeg
    (HasAdd, [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] HasAdd
    (HasSub, [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] HasSub
    (HasMul, [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] HasMul
    (HasDiv, [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] HasDiv
    (HasFold, [_, t]) -> getCandidate [t] HasFold
    (HasNatLits n, [t]) -> getCandidate [t] (HasNatLits n)
    (HasRatLits, [t]) -> getCandidate [t] HasRatLits
    (HasVecLits n, [_, t]) -> getCandidate [t] (HasVecLits n)
    (NatInDomainConstraint n, [t]) -> case argExpr t of
      VIndexType p size -> getCandidate [ExplicitArg p size] (NatInDomainConstraint n)
      _ -> []
    _ -> []

getCandidatesFromArgs :: ConstraintContext -> [NormArg] -> TypeClass -> [Candidate]
getCandidatesFromArgs ctx ts tc = catMaybes $ flip map ts $ \t -> do
  let e = argExpr t
  case getMeta (argExpr t) of
    Just m -> Just (Candidate m tc e ctx) -- m, t, tc)
    _ -> Nothing

auxiliaryTCError :: MonadCompile m => m a
auxiliaryTCError =
  compilerDeveloperError
    "Should not be considering defaults for auxiliary constraints"

ifTCError :: MonadCompile m => m a
ifTCError =
  compilerDeveloperError
    "Should not be considering defaults for 'HasIf' constraints"
