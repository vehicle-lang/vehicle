module Vehicle.Compile.Type.TypeClass.Defaults
  ( generateDefaultTypeClassSolution
  ) where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Language.Print (prettySimple)

--------------------------------------------------------------------------------
-- Default solutions to type-class constraints

-- This is some pretty ugly code. There must be a way of making this process
-- more elegant....

type Ctx = ConstraintContext

data DefaultFamily
  = NumFamily NumericType Bool Int
  | ConFamily Bool
  deriving (Eq, Ord)

sameFamily :: DefaultFamily -> DefaultFamily -> Bool
sameFamily NumFamily{} NumFamily{} = True
sameFamily ConFamily{} ConFamily{} = True
sameFamily _           _           = False

data Candidate = Candidate TypeClass CheckedExpr Ctx

data CandidateStatus
  = Valid Candidate
  | None
  | Invalid

instance Pretty CandidateStatus where
  pretty = \case
    Valid (Candidate tc _ _) -> pretty tc
    None                     -> "none encountered"
    Invalid                  -> "incompatible"

generateDefaultTypeClassSolution :: MonadConstraintSolving m
                                 => Meta
                                 -> m ConstraintProgress
generateDefaultTypeClassSolution meta =
  logCompilerPass ("search for default solution for" <+> pretty meta) $ do
    constraints <- getUnsolvedConstraints
    -- First group by common meta-variables
    strongestConstraint <- findStrongestConstraint meta constraints
    case strongestConstraint of
      None    -> return Stuck
      Invalid -> return Stuck
      Valid (Candidate tc metaExpr ctx) -> do
        let ann = inserted $ provenanceOf ctx
        solution <- defaultSolution ann (boundCtx $ varContext ctx) tc
        logDebug MaxDetail $
          "using default" <+> pretty meta <+> "=" <+> prettySimple solution <+>
          "         " <> parens ("from" <+> pretty tc)
        let newConstraint = UC ctx (Unify (metaExpr, solution))
        return $ Progress [newConstraint] mempty

findStrongestConstraint :: MonadCompile m
                        => Meta
                        -> [Constraint]
                        -> m CandidateStatus
findStrongestConstraint _ [] = return None
findStrongestConstraint meta (constraint : xs) = do
  recResult <- findStrongestConstraint meta xs
  case constraint of
    (TC ctx expr) -> case getDefaultCandidate expr of
      Nothing -> return recResult
      Just (m, mExpr, tc)
        | m /= meta -> return recResult
        | otherwise -> do
          logDebug MaxDetail $ "considering" <+> squotes (prettySimple constraint)
          let candidate = Candidate tc mExpr ctx
          let result = strongest candidate recResult
          incrCallDepth
          logDebug MaxDetail $ "status:" <+> pretty result
          decrCallDepth
          return result
    _ -> return recResult

strongest :: Candidate -> CandidateStatus -> CandidateStatus
strongest x                     None     = Valid x
strongest _                     Invalid  = Invalid
strongest x@(Candidate tc1 _ _) y@(Valid (Candidate tc2 _ _)) =
  case (family tc1, family tc2) of
    (c1, c2) | sameFamily c1 c2 -> if c1 > c2 then Valid x else y
    _                           -> Invalid
  where
  family :: TypeClass -> DefaultFamily
  family HasEq              = NumFamily Nat False 0
  family HasOrd             = NumFamily Nat False 0
  family HasAdd             = NumFamily Nat True  0
  family HasSub             = NumFamily Int True  0
  family HasMul             = NumFamily Nat True  0
  family HasDiv             = NumFamily Rat True  0
  family HasNeg             = NumFamily Int True  0
  family (HasNatLitsUpTo n) = NumFamily Nat False n
  family HasIntLits         = NumFamily Int False 0
  family HasRatLits         = NumFamily Rat False 0
  family HasConOps          = ConFamily False
  family HasConLitsOfSize{} = ConFamily True

defaultSolution :: MonadConstraintSolving m
                => Provenance
                -> BoundCtx
                -> TypeClass
                -> m CheckedExpr
defaultSolution ann _   HasEq               = return $ NatType ann
defaultSolution ann _   HasOrd              = return $ NatType ann
defaultSolution ann _   HasAdd              = return $ NatType ann
defaultSolution ann _   HasSub              = return $ IntType ann
defaultSolution ann _   HasMul              = return $ NatType ann
defaultSolution ann _   HasDiv              = return $ RatType ann
defaultSolution ann _   HasNeg              = return $ IntType ann
defaultSolution ann _   (HasNatLitsUpTo n)  = return $ mkIndexType ann (n + 1)
defaultSolution ann _   HasIntLits          = return $ IntType ann
defaultSolution ann _   HasRatLits          = return $ RatType ann
defaultSolution ann ctx HasConOps           = createDefaultListType ann ctx
defaultSolution ann ctx HasConLitsOfSize{}  = createDefaultListType ann ctx

createDefaultListType :: MonadConstraintSolving m => Provenance -> BoundCtx -> m CheckedExpr
createDefaultListType ann ctx = ListType ann . snd <$> freshMetaWith ann (Type ann 0) ctx

getDefaultCandidate :: TypeClassConstraint -> Maybe (Meta, CheckedExpr, TypeClass)
getDefaultCandidate (_ `Has` e) = case e of
  HasEqExpr            _     t -> extractMeta t HasEq
  HasOrdExpr           _     t -> extractMeta t HasOrd
  HasNatLitsUpToExpr   _ n   t -> extractMeta t (HasNatLitsUpTo n)
  HasIntLitsExpr       _     t -> extractMeta t HasIntLits
  HasRatLitsExpr       _     t -> extractMeta t HasRatLits
  HasArithOpExpr       _ tc  t -> extractMeta t tc
  HasConOpsExpr        _   _ t -> extractMeta t HasConOps
  HasConLitsOfSizeExpr _ n _ t -> extractMeta t (HasConLitsOfSize n)
  _                            -> Nothing

extractMeta :: CheckedExpr -> TypeClass -> Maybe (Meta, CheckedExpr, TypeClass)
extractMeta t tc = case exprHead t of
  (Meta _ m) -> Just (m, t, tc)
  _          -> Nothing