module Vehicle.Compile.Type.ConstraintSolver.TypeClassDefaults
  ( generateConstraintUsingDefaults
  ) where

import Data.Maybe (catMaybes)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Language.Print (prettySimple)
import Vehicle.Compile.Type.VariableContext
import Control.Monad (foldM)

--------------------------------------------------------------------------------
-- Default solutions to type-class constraints

-- This is some pretty ugly code. There must be a way of making this process
-- more elegant....

type Ctx = ConstraintContext

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
sameFamily NumericFamily{}   NumericFamily{}   = True
sameFamily ContainerFamily{} ContainerFamily{} = True
sameFamily BooleanFamily{}   BooleanFamily{}   = True
sameFamily PolarityFamily{}  PolarityFamily{}  = True
sameFamily LinearityFamily{} LinearityFamily{} = True
sameFamily _                 _                 = False

data Candidate = Candidate Meta TypeClass CheckedExpr Ctx

instance Pretty Candidate where
  pretty (Candidate m tc _ _) = pretty m <+> "~" <+> pretty tc

data CandidateStatus
  = Valid Candidate
  | None
  | Invalid

instance Pretty CandidateStatus where
  pretty = \case
    Valid c -> pretty c
    None    -> "none encountered"
    Invalid -> "incompatible"

generateConstraintUsingDefaults :: MonadMeta m
                                => [Constraint]
                                -> m (Maybe Constraint)
generateConstraintUsingDefaults constraints = do
  strongestConstraint <- findStrongestConstraint constraints
  case strongestConstraint of
    None    -> do
      logDebug MaxDetail "No default solution found"
      return Nothing
    Invalid -> return Nothing
    Valid (Candidate m tc metaExpr ctx) -> do
      let ann = inserted $ provenanceOf ctx
      solution <- defaultSolution ann (boundCtx $ varContext ctx) tc
      logDebug MaxDetail $
        "using default" <+> pretty m <+> "=" <+> prettySimple solution <+>
        "         " <> parens ("from" <+> pretty tc)
      let newConstraint = UC (copyContext ctx) (Unify (metaExpr, solution))
      return $ Just newConstraint

findStrongestConstraint :: MonadCompile m
                        => [Constraint]
                        -> m CandidateStatus
findStrongestConstraint [] = return None
findStrongestConstraint (constraint : xs) = do
  recResult <- findStrongestConstraint xs
  case constraint of
    (TC ctx expr) ->
      logCompilerSection MaxDetail ("considering" <+> squotes (prettySimple constraint)) $ do
        candidates <- getCandidatesFromConstraint ctx expr

        newStrongest <- foldM strongest recResult candidates
        logDebug MaxDetail $ indent 2 $ "status:" <+> pretty newStrongest
        return newStrongest

    _ -> return recResult

strongest :: MonadCompile m => CandidateStatus -> Candidate -> m CandidateStatus
strongest Invalid  _                       = return Invalid
strongest None     x                       = return $ Valid x
strongest y@(Valid (Candidate _ tc2 _ _)) x@(Candidate _ tc1 _ _) = do
  f1 <- familyOf tc1
  f2 <- familyOf tc2
  return $
    if not (sameFamily f1 f2)
      then y
    else if f1 > f2
      then Valid x
    else
      y

familyOf :: MonadCompile m => TypeClass -> m DefaultFamily
familyOf = \case
  HasNot                  -> return BooleanFamily
  HasAnd                  -> return BooleanFamily
  HasOr                   -> return BooleanFamily
  HasImplies              -> return BooleanFamily
  HasQuantifier{}         -> return BooleanFamily
  HasEq{}                 -> return $ NumericFamily NatT False 0
  HasOrd{}                -> return $ NumericFamily NatT False 0
  HasAdd                  -> return $ NumericFamily NatT True  0
  HasSub                  -> return $ NumericFamily IntT True  0
  HasMul                  -> return $ NumericFamily NatT True  0
  HasDiv                  -> return $ NumericFamily RatT True  0
  HasNeg                  -> return $ NumericFamily IntT True  0
  (HasNatLits n)          -> return $ NumericFamily NatT False n
  HasRatLits              -> return $ NumericFamily RatT False 0
  HasVecLits{}            -> return $ ContainerFamily True
  HasFold                 -> return $ ContainerFamily False
  HasQuantifierIn{}       -> return $ ContainerFamily False
  NatInDomainConstraint n -> return $ NumericFamily NatT False n

  MaxLinearity            -> auxiliaryTCError
  MulLinearity            -> auxiliaryTCError
  NegPolarity{}           -> auxiliaryTCError
  AddPolarity{}           -> auxiliaryTCError
  EqPolarity{}            -> auxiliaryTCError
  ImpliesPolarity{}       -> auxiliaryTCError
  MaxPolarity{}           -> auxiliaryTCError
  AlmostEqualConstraint{} -> auxiliaryTCError
  FunctionLinearity{}     -> auxiliaryTCError
  FunctionPolarity{}      -> auxiliaryTCError

defaultSolution :: MonadMeta m
                => Provenance
                -> TypingBoundCtx
                -> TypeClass
                -> m CheckedExpr
defaultSolution p ctx = \case
  HasEq{}                 -> return $ NatType p
  HasOrd{}                -> return $ NatType p
  HasNot                  -> createDefaultBoolType p
  HasAnd                  -> createDefaultBoolType p
  HasOr                   -> createDefaultBoolType p
  HasImplies              -> createDefaultBoolType p
  HasQuantifier{}         -> createDefaultBoolType p
  HasAdd                  -> return $ NatType p
  HasSub                  -> return $ IntType p
  HasMul                  -> return $ NatType p
  HasDiv                  -> createDefaultRatType p
  HasNeg                  -> return $ IntType p
  HasNatLits n            -> return $ mkIndexType p (n + 1)
  HasRatLits              -> createDefaultRatType p
  HasVecLits{}            -> createDefaultListType p ctx
  HasFold                 -> createDefaultListType p ctx
  HasQuantifierIn{}       -> createDefaultListType p ctx
  NatInDomainConstraint n -> return $ NatLiteral p (n + 1)

  MaxLinearity            -> auxiliaryTCError
  MulLinearity            -> auxiliaryTCError
  NegPolarity{}           -> auxiliaryTCError
  AddPolarity{}           -> auxiliaryTCError
  EqPolarity{}            -> auxiliaryTCError
  ImpliesPolarity{}       -> auxiliaryTCError
  MaxPolarity{}           -> auxiliaryTCError
  AlmostEqualConstraint{} -> auxiliaryTCError
  FunctionLinearity{}     -> auxiliaryTCError
  FunctionPolarity{}      -> auxiliaryTCError

createDefaultListType :: MonadMeta m => Provenance -> TypingBoundCtx -> m CheckedType
createDefaultListType p ctx = do
  tElem <- freshExprMeta p (TypeUniverse p 0) ctx
  return $ ListType p tElem

createDefaultBoolType :: MonadMeta m => Provenance -> m CheckedType
createDefaultBoolType p = do
  lin <- freshLinearityMeta p
  pol <- freshPolarityMeta p
  return $ AnnBoolType p lin pol

createDefaultRatType :: MonadMeta m => Provenance -> m CheckedType
createDefaultRatType p = do
  lin <- freshLinearityMeta p
  return $ AnnRatType p lin

getCandidatesFromConstraint :: MonadCompile m => Ctx -> TypeClassConstraint -> m [Candidate]
getCandidatesFromConstraint ctx (Has _ tc args) = do
  let getCandidate = getCandidatesFromArgs ctx
  return $ case (tc, args) of
    (HasEq eq,     [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] (HasEq eq)
    (HasOrd ord,   [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] (HasOrd ord)
    (HasNeg,       [tArg, _tRes])         -> getCandidate [tArg]         HasNeg
    (HasAdd,       [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] HasAdd
    (HasSub,       [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] HasSub
    (HasMul,       [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] HasMul
    (HasDiv,       [tArg1, tArg2, _tRes]) -> getCandidate [tArg1, tArg2] HasDiv
    (HasFold,      [_, t])                -> getCandidate [t] HasFold
    (HasNatLits n, [t])                   -> getCandidate [t] (HasNatLits n)
    (HasRatLits,   [t])                   -> getCandidate [t] HasRatLits
    (HasVecLits n, [_, t])                -> getCandidate [t] (HasVecLits n)
    (NatInDomainConstraint n, [t]) -> case argExpr t of
      BuiltinExpr _ Index [size] -> getCandidate [size] (NatInDomainConstraint n)
      _                          -> []
    _                                     -> []

getCandidatesFromArgs :: Ctx -> [CheckedArg] -> TypeClass -> [Candidate]
getCandidatesFromArgs ctx ts tc = catMaybes $ flip map ts $ \t -> do
  let e = argExpr t
  case exprHead (argExpr t) of
    (Meta _ m) -> Just (Candidate m tc e ctx) --m, t, tc)
    _          -> Nothing

auxiliaryTCError :: MonadCompile m => m a
auxiliaryTCError = compilerDeveloperError
  "Should not be considering defaults for auxiliary constraints"