module Vehicle.Compile.Type.TypeClass.Defaults
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
                                -> m ConstraintProgress
generateConstraintUsingDefaults constraints = do
  strongestConstraint <- findStrongestConstraint constraints
  case strongestConstraint of
    None    -> do
      logDebug MaxDetail "No default solution found"
      return Stuck
    Invalid -> return Stuck
    Valid (Candidate m tc metaExpr ctx) -> do
      let ann = inserted $ provenanceOf ctx
      solution <- defaultSolution ann (boundCtx $ varContext ctx) tc
      logDebug MaxDetail $
        "using default" <+> pretty m <+> "=" <+> prettySimple solution <+>
        "         " <> parens ("from" <+> pretty tc)
      let newConstraint = UC ctx (Unify (metaExpr, solution))
      return $ Progress [newConstraint] mempty

findStrongestConstraint :: MonadCompile m
                        => [Constraint]
                        -> m CandidateStatus
findStrongestConstraint [] = return None
findStrongestConstraint (constraint : xs) = do
  recResult <- findStrongestConstraint xs
  case constraint of
    (TC ctx expr) -> do

      logDebug MaxDetail $ "considering" <+> squotes (prettySimple constraint)
      incrCallDepth

      candidates <- getCandidatesFromConstraint ctx expr
      logDebug MaxDetail $ pretty candidates

      newStrongest <- foldM strongest recResult candidates
      logDebug MaxDetail $ indent 2 $ "status:" <+> pretty newStrongest
      decrCallDepth
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
  HasNot             -> return BooleanFamily
  HasAnd             -> return BooleanFamily
  HasOr              -> return BooleanFamily
  HasImpl            -> return BooleanFamily
  HasQuantifier{}    -> return BooleanFamily
  HasEq{}            -> return $ NumericFamily Nat False 0
  HasOrd{}           -> return $ NumericFamily Nat False 0
  HasAdd             -> return $ NumericFamily Nat True  0
  HasSub             -> return $ NumericFamily Int True  0
  HasMul             -> return $ NumericFamily Nat True  0
  HasDiv             -> return $ NumericFamily Rat True  0
  HasNeg             -> return $ NumericFamily Int True  0
  (HasNatLitsUpTo n) -> return $ NumericFamily Nat False n
  HasIntLits         -> return $ NumericFamily Int False 0
  HasRatLits         -> return $ NumericFamily Rat False 0
  HasFold            -> return $ ContainerFamily False
  HasQuantifierIn{}  -> return $ ContainerFamily False
  HasConLitsOfSize{} -> return $ ContainerFamily True

  MaxLinearity                        -> auxiliaryTCError
  MulLinearity                        -> auxiliaryTCError
  NegPolarity{}                       -> auxiliaryTCError
  AddPolarity{}                       -> auxiliaryTCError
  EqPolarity{}                        -> auxiliaryTCError
  ImplPolarity{}                      -> auxiliaryTCError
  MaxPolarity{}                       -> auxiliaryTCError
  TypesEqualModAuxiliaryAnnotations{} -> auxiliaryTCError

defaultSolution :: MonadMeta m
                => Provenance
                -> TypingBoundCtx
                -> TypeClass
                -> m CheckedExpr
defaultSolution ann ctx = \case
  HasEq{}            -> return $ NatType ann
  HasOrd{}           -> return $ NatType ann
  HasNot             -> createDefaultBoolType ann
  HasAnd             -> createDefaultBoolType ann
  HasOr              -> createDefaultBoolType ann
  HasImpl            -> createDefaultBoolType ann
  HasQuantifier{}    -> createDefaultBoolType ann
  HasAdd             -> return $ NatType ann
  HasSub             -> return $ IntType ann
  HasMul             -> return $ NatType ann
  HasDiv             -> createDefaultRatType ann
  HasNeg             -> return $ IntType ann
  (HasNatLitsUpTo n) -> return $ mkIndexType ann (n + 1)
  HasIntLits         -> return $ IntType ann
  HasRatLits         -> createDefaultRatType ann
  HasFold            -> createDefaultListType ann ctx
  HasQuantifierIn{}  -> createDefaultListType ann ctx
  HasConLitsOfSize{} -> createDefaultListType ann ctx

  MaxLinearity                        -> auxiliaryTCError
  MulLinearity                        -> auxiliaryTCError
  NegPolarity{}                       -> auxiliaryTCError
  AddPolarity{}                       -> auxiliaryTCError
  EqPolarity{}                        -> auxiliaryTCError
  ImplPolarity{}                      -> auxiliaryTCError
  MaxPolarity{}                       -> auxiliaryTCError
  TypesEqualModAuxiliaryAnnotations{} -> auxiliaryTCError

createDefaultListType :: MonadMeta m => Provenance -> TypingBoundCtx -> m CheckedExpr
createDefaultListType p ctx = do
  tElem <- freshExprMeta p (TypeUniverse p 0) ctx
  return $ ListType p tElem

createDefaultBoolType :: MonadMeta m => Provenance -> m CheckedExpr
createDefaultBoolType p = do
  lin <- freshLinearityMeta p
  pol <- freshPolarityMeta p
  return $ AnnBoolType p lin pol

createDefaultRatType :: MonadMeta m => Provenance -> m CheckedExpr
createDefaultRatType p = do
  lin <- freshLinearityMeta p
  return $ AnnRatType p lin

getCandidatesFromConstraint :: MonadCompile m => Ctx -> TypeClassConstraint -> m [Candidate]
getCandidatesFromConstraint ctx (_ `Has` e) = do
  let getCandidate = getCandidatesFromArgs ctx
  return $ case e of
    HasEqExpr            _ eq  tArg1 tArg2 _tRes -> getCandidate [tArg1, tArg2] (HasEq eq)
    HasOrdExpr           _ ord tArg1 tArg2 _tRes -> getCandidate [tArg1, tArg2] (HasOrd ord)
    HasNegExpr           _     tArg        _tRes -> getCandidate [tArg]         HasNeg
    HasAddExpr           _     tArg1 tArg2 _tRes -> getCandidate [tArg1, tArg2] HasAdd
    HasSubExpr           _     tArg1 tArg2 _tRes -> getCandidate [tArg1, tArg2] HasSub
    HasMulExpr           _     tArg1 tArg2 _tRes -> getCandidate [tArg1, tArg2] HasMul
    HasDivExpr           _     tArg1 tArg2 _tRes -> getCandidate [tArg1, tArg2] HasDiv
    HasFoldExpr          _   _ t                 -> getCandidate [t] HasFold
    HasNatLitsUpToExpr   _ n   t                 -> getCandidate [t] (HasNatLitsUpTo n)
    HasIntLitsExpr       _     t                 -> getCandidate [t] HasIntLits
    HasRatLitsExpr       _     t                 -> getCandidate [t] HasRatLits
    HasConLitsOfSizeExpr _ n _ t                 -> getCandidate [t] (HasConLitsOfSize n)
    _                                            -> []

getCandidatesFromArgs :: Ctx -> [CheckedExpr] -> TypeClass -> [Candidate]
getCandidatesFromArgs ctx ts tc = catMaybes $ flip map ts $ \t ->
  case exprHead t of
    (Meta _ m) -> Just (Candidate m tc t ctx) --m, t, tc)
    _          -> Nothing

auxiliaryTCError :: MonadCompile m => m a
auxiliaryTCError = compilerDeveloperError
  "Should not be considering defaults for auxiliary constraints"