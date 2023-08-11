{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard.Constraint.InstanceDefaults where

import Data.Maybe (fromMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint.Core (parseInstanceGoal)
import Vehicle.Compile.Type.Constraint.InstanceDefaultSolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Expr.Normalised (Value (..), isNMeta)

instance HasInstanceDefaults StandardBuiltin where
  getCandidatesFromConstraint = getCandidates
  compareCandidates = compareCandidate

--------------------------------------------------------------------------------
-- Default solutions to type-class constraints

compareCandidate :: Candidate StandardBuiltin -> Candidate StandardBuiltin -> Maybe Ordering
compareCandidate c1 c2 = do
  let f1 = familyOf (candidateTypeClass c1)
  let f2 = familyOf (candidateTypeClass c2)
  if not (sameFamily f1 f2)
    then Nothing
    else
      if f1 > f2
        then Just GT
        else
          if f1 < f2
            then Just LT
            else Just EQ

-- This is some pretty ugly code. There must be a way of making this process
-- more elegant....

data NumericType
  = NatT
  | IntT
  | RatT
  deriving (Eq, Ord)

data DefaultFamily
  = NumericFamily NumericType Bool
  | ContainerFamily Bool
  | BooleanFamily
  | Other
  deriving (Eq, Ord)

sameFamily :: DefaultFamily -> DefaultFamily -> Bool
sameFamily NumericFamily {} NumericFamily {} = True
sameFamily ContainerFamily {} ContainerFamily {} = True
sameFamily BooleanFamily {} BooleanFamily {} = True
sameFamily _ _ = False

familyOf :: StandardBuiltin -> DefaultFamily
familyOf = \case
  TypeClass HasQuantifier {} -> BooleanFamily
  TypeClass HasEq {} -> NumericFamily NatT False
  TypeClass HasOrd {} -> NumericFamily NatT False
  TypeClass HasAdd -> NumericFamily NatT True
  TypeClass HasSub -> NumericFamily IntT True
  TypeClass HasMul -> NumericFamily NatT True
  TypeClass HasDiv -> NumericFamily RatT True
  TypeClass HasNeg -> NumericFamily IntT True
  TypeClass HasNatLits -> NumericFamily NatT False
  TypeClass HasRatLits -> NumericFamily RatT False
  TypeClass HasVecLits {} -> ContainerFamily True
  TypeClass HasMap -> ContainerFamily False
  TypeClass HasFold -> ContainerFamily False
  TypeClass HasQuantifierIn {} -> ContainerFamily False
  NatInDomainConstraint -> NumericFamily NatT False
  _ -> Other

getCandidates ::
  forall m.
  (MonadCompile m) =>
  StandardConstraintContext ->
  StandardInstanceConstraint ->
  m [Candidate StandardBuiltin]
getCandidates ctx (Resolve origin _ _ expr) = do
  InstanceGoal {..} <- parseInstanceGoal expr
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

  return $ case defaults of
    Nothing -> []
    Just (defaultValue, defaultArgs) ->
      getCandidatesFromArgs (ctx, origin) goalHead defaultValue defaultArgs

getCandidatesFromArgs ::
  InstanceConstraintInfo StandardBuiltin ->
  StandardBuiltin ->
  StandardNormExpr ->
  StandardSpine ->
  [Candidate StandardBuiltin]
getCandidatesFromArgs info tc solution ts = map mkCandidate (filter (isNMeta . argExpr) ts)
  where
    mkCandidate t =
      Candidate
        { candidateTypeClass = tc,
          candidateMetaExpr = argExpr t,
          candidateInfo = info,
          candidateSolution = solution
        }
