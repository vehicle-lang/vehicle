{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use id" #-}

module Vehicle.Data.Builtin.Standard.Instances
  ( standardBuiltinInstances,
  )
where

import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core (InstanceCandidate (..), InstanceDatabase (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL
import Vehicle.Prelude

standardBuiltinInstances :: InstanceDatabase Builtin
standardBuiltinInstances = makeInstanceDatabase allInstances

-- All other type-classes and their instances are declared in the standard
-- library (`lib/Definitions.vcl`). The ones that remain here cannot be moved
-- there because:
--
--   * `HasNatLits`, `HasRatLits` and `HasVecLits` are coercions inserted
--     around every literal during desugaring (including the literals in the
--     standard library itself). They are treated specially by the type-checker
--     (see `isCastConstraint`), the `Index` instance must inspect the value of
--     the literal via the `NatInDomainConstraint`, vector literals are variadic,
--     and their solutions are casts that have no surface syntax.
--
--   * `IsTensorType` is what gives meaning to the `Tensor` keyword, so any
--     standard library declaration of it would itself have to use `Tensor`.
--     Its solutions are types rather than records of operations, and the
--     decidability type-system relies on its own builtin version of it.
allInstances :: [InstanceCandidate Builtin]
allInstances =
  mkCandidate
    <$> [
          ----------------
          -- HasRatLits --
          ----------------
          ( hasRatLits (tRatTensor dimNil),
            builtinCast (FromRat FromRatToRat),
            Nothing
          ),
          ----------------
          -- HasNatLits --
          ----------------
          ( forAllIrrelevantNat "n" $ \n ->
              hasNatLits (tIndex n),
            irrelImplNatLam "n" $ \n ->
              builtinCast (FromNat FromNatToIndex) .@@@ [n],
            Nothing
          ),
          ( hasNatLits tNat,
            builtinCast (FromNat FromNatToNat),
            Just 0
          ),
          ( hasNatLits (tRatTensor dimNil),
            builtinCast (FromNat FromNatToRat),
            Nothing
          ),
          ----------------
          -- HasVecLits --
          ----------------
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                forAllDims $ \ds ->
                  hasVecLits (tTensor t (dimCons d ds)) (tTensor t ds) d,
            implLam "t" type0 $ \t ->
              lamDim $ \d ->
                lamDims $ \ds ->
                  builtinFunction StackTensor @@@ [t, d] .@@@ [ds],
            Nothing
          ),
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                hasVecLits (tVector t d) t d,
            implLam "t" type0 $ \t ->
              lamDim $ \d ->
                builtinConstructor VectorLiteral @@@ [t, d],
            Nothing
          ),
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                hasVecLits (tList t) t d,
            implLam "t" type0 $ \t ->
              lamDim $ \d ->
                builtinCast FromVectorToList @@@ [t, d],
            Nothing
          ),
          ------------------
          -- IsTensorType --
          ------------------
          ( forAllDims $ \ds ->
              isTensorType tBool ds,
            lamDims $ \ds ->
              tTensor tBool ds,
            Nothing
          ),
          ( forAllDims $ \ds ->
              isTensorType tRat ds,
            lamDims $ \ds ->
              tTensor tRat ds,
            Nothing
          ),
          ( forAllIrrelevant "ds" tDims $ \ds ->
              forAllTypes $ \t ->
                isTensorType (tTensor t dimNil) ds,
            lamDims $ \ds ->
              implLam "t" type0 $ \t ->
                tTensor t ds,
            Nothing
          )
        ]
