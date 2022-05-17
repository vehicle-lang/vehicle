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
  let constraintsByMeta = Map.mapMaybe id $ groupByMetas constraints
  newConstraints <- forM (Map.assocs constraintsByMeta) $ \(meta, (tc, (metaExpr, ctx))) -> do
    logDebug MaxDetail $ "Using default for" <+> pretty meta <+> "=" <+> pretty tc
    let ann = inserted $ provenanceOf ctx
    solution <- defaultSolution ann (boundCtx $ varContext ctx) tc
    return $ UC ctx (Unify (metaExpr, solution))

  return $ if null newConstraints
    then Stuck
    else Progress newConstraints mempty

groupByMetas :: [(TypeClassConstraint, Ctx)]
             -> Map Meta (Maybe (TypeClass, (CheckedExpr, Ctx)))
groupByMetas []       = mempty
groupByMetas ((x, ctx) : xs) = case getDefaultCandidate x of
  Nothing             -> groupByMetas xs
  Just (m, mExpr, tc) -> Map.insertWith merge m (Just (tc, (mExpr, ctx))) (groupByMetas xs)
  where
    merge :: Maybe (TypeClass, a) -> Maybe (TypeClass, a) -> Maybe (TypeClass, a)
    merge (Just tc1) (Just tc2) = strongest tc1 tc2
    merge _          _          = Nothing

strongest :: (TypeClass, a) -> (TypeClass, a) -> Maybe (TypeClass, a)
strongest x@(tc1, _) y@(tc2, _) = case (numType tc1, numType tc2) of
  (Just c1, Just c2)
    | sameFamily c1 c2 -> Just $ if c1 > c2 then x else y
  _                    -> Nothing
  where
  numType :: TypeClass -> Maybe DefaultFamily
  numType HasNatOps          = Just $ NumFamily Nat True  0
  numType HasIntOps          = Just $ NumFamily Int True  0
  numType HasRatOps          = Just $ NumFamily Rat True  0
  numType (HasNatLitsUpTo n) = Just $ NumFamily Nat False n
  numType HasIntLits         = Just $ NumFamily Int False 0
  numType HasRatLits         = Just $ NumFamily Rat False 0
  numType HasConOps          = Just $ ConFamily True
  numType HasConLitsOfSize{} = Just $ ConFamily False
  numType _                  = Nothing

defaultSolution :: MonadConstraintSolving m
                => CheckedAnn
                -> BoundCtx
                -> TypeClass
                -> m CheckedExpr
defaultSolution ann _   HasNatOps           = return $ NatType ann
defaultSolution ann _   HasIntOps           = return $ IntType ann
defaultSolution ann _   HasRatOps           = return $ RatType ann
defaultSolution ann _   (HasNatLitsUpTo n)  = return $ mkIndexType ann (n + 1)
defaultSolution ann _   HasIntLits          = return $ IntType ann
defaultSolution ann _   HasRatLits          = return $ RatType ann
defaultSolution ann ctx HasConOps           = ListType ann <$> (snd <$> freshMetaWith ann ctx)
defaultSolution ann ctx HasConLitsOfSize{}  = ListType ann <$> (snd <$> freshMetaWith ann ctx)
defaultSolution _   _   tc                  = compilerDeveloperError $
  "TypeClass" <+> pretty tc <+> "should have already been eliminated"

getDefaultCandidate :: TypeClassConstraint -> Maybe (Meta, CheckedExpr, TypeClass)
getDefaultCandidate (_ `Has` e) = case e of
  HasNatLitsUpToExpr   _ n   t -> extractMeta t (HasNatLitsUpTo n)
  HasIntLitsExpr       _     t -> extractMeta t HasIntLits
  HasRatLitsExpr       _     t -> extractMeta t HasRatLits
  HasNatOpsExpr        _     t -> extractMeta t HasNatOps
  HasIntOpsExpr        _     t -> extractMeta t HasIntOps
  HasRatOpsExpr        _     t -> extractMeta t HasRatOps
  HasConOpsExpr        _   _ t -> extractMeta t HasConOps
  HasConLitsOfSizeExpr _ n _ t -> extractMeta t (HasConLitsOfSize n)
  _                            -> Nothing

extractMeta :: CheckedExpr -> TypeClass -> Maybe (Meta, CheckedExpr, TypeClass)
extractMeta t tc = case exprHead t of
  (Meta _ m) -> Just (m, t, tc)
  _          -> Nothing