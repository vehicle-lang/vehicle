module Vehicle.Compile.Type.Defaults
  ( solveDefaultTypeClassConstraints
  ) where

import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad (forM)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Language.Print (prettySimple)
import Data.Maybe (fromMaybe)

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

solveDefaultTypeClassConstraints :: MonadConstraintSolving m
                                 => [(TypeClassConstraint, Ctx)]
                                 -> m ConstraintProgress
solveDefaultTypeClassConstraints constraints = do
  -- First group by common meta-variables
  constraintsByMeta <- Map.mapMaybe id <$> groupByMetas constraints
  newConstraints <- forM (Map.assocs constraintsByMeta) $ \(meta, (tc, (metaExpr, ctx))) -> do
    let ann = inserted $ provenanceOf ctx
    solution <- defaultSolution ann (boundCtx $ varContext ctx) tc
    logDebug MaxDetail $
      "using default " <+> pretty meta <+> "=" <+> prettySimple solution <+>
      parens ("from" <+> pretty tc)
    return $ UC ctx (Unify (metaExpr, solution))

  logDebug MaxDetail ""

  return $ if null newConstraints
    then Stuck
    else Progress newConstraints mempty

groupByMetas :: MonadCompile m
             => [(TypeClassConstraint, Ctx)]
             -> m (Map Meta (Maybe (TypeClass, (CheckedExpr, Ctx))))
groupByMetas []       = return mempty
groupByMetas ((x, ctx) : xs) = case getDefaultCandidate x of
  Nothing             -> do
    logDebug MaxDetail $ "discarding" <+> squotes (prettySimple (TC ctx x))
    groupByMetas xs
  Just (m, mExpr, tc) -> do
    recResult <- groupByMetas xs
    logDebug MaxDetail $ "considering" <+> squotes (prettySimple (TC ctx x))
    let result = Map.insertWith merge m (Just (tc, (mExpr, ctx))) recResult
    incrCallDepth
    let element = fmap fst (fromMaybe Nothing (Map.lookup m result))
    logDebug MaxDetail $ "current for" <+> pretty m <> ":" <+> pretty (show element)
    decrCallDepth
    return result
  where
    merge :: Maybe (TypeClass, a) -> Maybe (TypeClass, a) -> Maybe (TypeClass, a)
    merge (Just tc1) (Just tc2) = strongest tc1 tc2
    merge _          _          = Nothing

strongest :: (TypeClass, a) -> (TypeClass, a) -> Maybe (TypeClass, a)
strongest x@(tc1, _) y@(tc2, _) = case (family tc1, family tc2) of
  (c1, c2) | sameFamily c1 c2 -> Just $ if c1 > c2 then x else y
  _                           -> Nothing
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
                => CheckedAnn
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
defaultSolution ann ctx HasConOps           = ListType ann <$> (snd <$> freshMetaWith ann ctx)
defaultSolution ann ctx HasConLitsOfSize{}  = ListType ann <$> (snd <$> freshMetaWith ann ctx)

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