module Vehicle.Compile.Type.Subsystem.Standard.Interface where

import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Expr.DSL
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Printing builtins

class (Show builtin, Eq builtin) => PrintableBuiltin builtin where
  -- | Convert expressions with the builtin back to expressions with the standard
  -- builtin type. Used for printing.
  convertBuiltin ::
    Provenance ->
    builtin ->
    Expr var Builtin

  isCoercion ::
    builtin ->
    Bool

instance PrintableBuiltin Builtin where
  convertBuiltin = Builtin

  isCoercion = \case
    BuiltinFunction FromNat {} -> True
    BuiltinFunction FromRat {} -> True
    TypeClassOp FromNatTC {} -> True
    TypeClassOp FromRatTC {} -> True
    TypeClassOp FromVecTC {} -> True
    _ -> False

--------------------------------------------------------------------------------
-- Interface to standard builtins

-- At various points in the compiler, we have different sets of builtins (e.g.
-- first time we type-check we use the standard set of builtins + type +
-- type classes, but when checking polarity and linearity information we
-- subsitute out all the types and type-classes for new types.)
--
-- The interfaces defined in this file allow us to abstract over the exact set
-- of builtins being used, and therefore allows us to define operations
-- (e.g. normalisation) once, rather than once for each builtin type.

-- | Indicates that this set of builtins has the standard builtin constructors
-- and functions.
class HasStandardData builtin where
  mkBuiltinConstructor :: BuiltinConstructor -> builtin
  getBuiltinConstructor :: builtin -> Maybe BuiltinConstructor

  mkBuiltinFunction :: BuiltinFunction -> builtin
  getBuiltinFunction :: builtin -> Maybe BuiltinFunction

  isTypeClassOp :: builtin -> Bool

-- | Indicates that this set of builtins has the standard set of types.
class HasStandardTypes builtin where
  mkBuiltinType :: BuiltinType -> builtin
  getBuiltinType :: builtin -> Maybe BuiltinType

  mkNatInDomainConstraint :: builtin

-- | Indicates that this set of builtins has the standard set of constructors,
-- functions and types.
type HasStandardBuiltins builtin =
  ( HasStandardTypes builtin,
    HasStandardData builtin
  )

instance HasStandardTypes Builtin where
  mkBuiltinType = BuiltinType
  getBuiltinType = \case
    BuiltinType c -> Just c
    _ -> Nothing

  mkNatInDomainConstraint = NatInDomainConstraint

instance HasStandardData Builtin where
  mkBuiltinFunction = BuiltinFunction
  getBuiltinFunction = \case
    BuiltinFunction c -> Just c
    _ -> Nothing

  mkBuiltinConstructor = BuiltinConstructor
  getBuiltinConstructor = \case
    BuiltinConstructor c -> Just c
    _ -> Nothing

  isTypeClassOp = \case
    TypeClassOp {} -> True
    _ -> False

--------------------------------------------------------------------------------
-- Types DSL

builtinType :: (HasStandardTypes builtin) => BuiltinType -> DSLExpr builtin
builtinType = builtin . mkBuiltinType

tUnit :: (HasStandardTypes builtin) => DSLExpr builtin
tUnit = builtinType Unit

tBool, tNat, tInt, tRat :: (HasStandardTypes builtin) => DSLExpr builtin
tBool = builtinType Bool
tNat = builtinType Nat
tInt = builtinType Int
tRat = builtinType Rat

tVector :: (HasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
tVector tElem dim = builtinType Vector @@ [tElem] .@@ [dim]

tVectorFunctor :: (HasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tVectorFunctor n = explLam "A" type0 (`tVector` n)

tListRaw :: (HasStandardTypes builtin) => DSLExpr builtin
tListRaw = builtinType List

tList :: (HasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tList tElem = tListRaw @@ [tElem]

tIndex :: (HasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tIndex n = builtinType Index .@@ [n]

forAllNat :: (HasStandardTypes builtin) => (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllNat = forAll "n" tNat

forAllIrrelevantNat :: (HasStandardTypes builtin) => Name -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllIrrelevantNat name = pi (Just name) (Implicit False) Irrelevant tNat

irrelImplNatLam :: (HasStandardTypes builtin) => Name -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
irrelImplNatLam n = lam n (Implicit False) Irrelevant tNat

--------------------------------------------------------------------------------
-- Type exprs

pattern NullaryTypeExpr :: (HasStandardTypes builtin) => Provenance -> BuiltinType -> Expr var builtin
pattern NullaryTypeExpr p b <- Builtin p (getBuiltinType -> Just b)
  where
    NullaryTypeExpr p b = Builtin p (mkBuiltinType b)

pattern TypeExpr ::
  (HasStandardTypes builtin) =>
  Provenance ->
  BuiltinType ->
  NonEmpty (Arg var builtin) ->
  Expr var builtin
pattern TypeExpr p b args <- BuiltinExpr p (getBuiltinType -> Just b) args
  where
    TypeExpr p b args = BuiltinExpr p (mkBuiltinType b) args

pattern BoolType :: (HasStandardTypes builtin) => Provenance -> Expr var builtin
pattern BoolType p = NullaryTypeExpr p Bool

pattern NatType :: (HasStandardTypes builtin) => Provenance -> Expr var builtin
pattern NatType p = NullaryTypeExpr p Nat

pattern IntType :: (HasStandardTypes builtin) => Provenance -> Expr var builtin
pattern IntType p = NullaryTypeExpr p Int

pattern RatType :: (HasStandardTypes builtin) => Provenance -> Expr var builtin
pattern RatType p = NullaryTypeExpr p Rat

pattern ListType :: (HasStandardTypes builtin) => Provenance -> Expr var builtin -> Expr var builtin
pattern ListType p tElem <- TypeExpr p List [RelevantExplicitArg _ tElem]

pattern RawListType :: (HasStandardTypes builtin) => Provenance -> Expr var builtin
pattern RawListType p = NullaryTypeExpr p List

pattern VectorType ::
  (HasStandardTypes builtin) =>
  Provenance ->
  Expr var builtin ->
  Expr var builtin ->
  Expr var builtin
pattern VectorType p tElem tDim <-
  TypeExpr
    p
    Vector
    [ RelevantExplicitArg _ tElem,
      IrrelevantExplicitArg _ tDim
      ]
  where
    VectorType p tElem tDim =
      TypeExpr
        p
        Vector
        [ RelevantExplicitArg p tElem,
          IrrelevantExplicitArg p tDim
        ]

--------------------------------------------------------------------------------
-- Type values

pattern VBuiltinType :: (HasStandardTypes builtin) => BuiltinType -> Spine builtin -> VType builtin
pattern VBuiltinType c args <- VBuiltin (getBuiltinType -> Just c) args
  where
    VBuiltinType c args = VBuiltin (mkBuiltinType c) args

pattern VBoolType :: (HasStandardTypes builtin) => VType builtin
pattern VBoolType <- VBuiltinType Bool []
  where
    VBoolType = VBuiltinType Bool []

pattern VIndexType :: (HasStandardTypes builtin) => VType builtin -> VType builtin
pattern VIndexType size <- VBuiltinType Index [IrrelevantExplicitArg _ size]
  where
    VIndexType size = VBuiltinType Index [IrrelevantExplicitArg mempty size]

pattern VNatType :: (HasStandardTypes builtin) => VType builtin
pattern VNatType <- VBuiltinType Nat []
  where
    VNatType = VBuiltinType Nat []

pattern VIntType :: (HasStandardTypes builtin) => VType builtin
pattern VIntType <- VBuiltinType Int []
  where
    VIntType = VBuiltinType Int []

pattern VRatType :: (HasStandardTypes builtin) => VType builtin
pattern VRatType <- VBuiltinType Rat []
  where
    VRatType = VBuiltinType Rat []

pattern VRawListType :: (HasStandardTypes builtin) => VType builtin
pattern VRawListType <- VBuiltinType List []
  where
    VRawListType = VBuiltinType List []

pattern VListType :: (HasStandardTypes builtin) => VType builtin -> VType builtin
pattern VListType tElem <- VBuiltinType List [RelevantExplicitArg _ tElem]

pattern VVectorType :: (HasStandardTypes builtin) => VType builtin -> Value builtin -> VType builtin
pattern VVectorType tElem dim <- VBuiltinType Vector [RelevantExplicitArg _ tElem, IrrelevantExplicitArg _ dim]
  where
    VVectorType tElem dim = VBuiltinType Vector [RelevantExplicitArg mempty tElem, IrrelevantExplicitArg mempty dim]

pattern VTensorType :: (HasStandardTypes builtin) => VType builtin -> Value builtin -> VType builtin
pattern VTensorType tElem dims <-
  VFreeVar TensorIdent [RelevantExplicitArg _ tElem, RelevantExplicitArg _ dims]

--------------------------------------------------------------------------------
-- Constructors DSL

builtinConstructor :: (HasStandardData builtin) => BuiltinConstructor -> DSLExpr builtin
builtinConstructor = builtin . mkBuiltinConstructor

nil :: (HasStandardData builtin) => DSLExpr builtin -> DSLExpr builtin
nil tElem = builtinConstructor Nil @@@ [tElem]

cons :: (HasStandardData builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
cons tElem x xs = builtinConstructor Cons @@@ [tElem] @@ [x, xs]

natLit :: (HasStandardData builtin) => Int -> DSLExpr builtin
natLit n = builtinConstructor (LNat n)

boolLit :: (HasStandardData builtin) => Bool -> DSLExpr builtin
boolLit n = builtinConstructor (LBool n)

ratLit :: (HasStandardData builtin) => Rational -> DSLExpr builtin
ratLit r = builtinConstructor (LRat r)

unitLit :: (HasStandardData builtin) => DSLExpr builtin
unitLit = builtinConstructor LUnit

--------------------------------------------------------------------------------
-- Constraints DSL

natInDomainConstraint ::
  (HasStandardTypes builtin) =>
  DSLExpr builtin ->
  DSLExpr builtin ->
  DSLExpr builtin
natInDomainConstraint n t = builtin mkNatInDomainConstraint @@ [n, t]

--------------------------------------------------------------------------------
-- Expr constructors patterns

pattern UnitLiteral :: (HasStandardData builtin) => Provenance -> Expr var builtin
pattern UnitLiteral p <- Builtin p (getBuiltinConstructor -> Just LUnit)
  where
    UnitLiteral p = Builtin p (mkBuiltinConstructor LUnit)

pattern BoolLiteral :: (HasStandardData builtin) => Provenance -> Bool -> Expr var builtin
pattern BoolLiteral p n <- Builtin p (getBuiltinConstructor -> Just (LBool n))
  where
    BoolLiteral p n = Builtin p (mkBuiltinConstructor (LBool n))

pattern TrueExpr :: (HasStandardData builtin) => Provenance -> Expr var builtin
pattern TrueExpr p = BoolLiteral p True

pattern FalseExpr :: (HasStandardData builtin) => Provenance -> Expr var builtin
pattern FalseExpr p = BoolLiteral p False

pattern IndexLiteral :: (HasStandardData builtin) => Provenance -> Int -> Expr var builtin
pattern IndexLiteral p n <- Builtin p (getBuiltinConstructor -> Just (LIndex n))
  where
    IndexLiteral p n = Builtin p (mkBuiltinConstructor (LIndex n))

pattern NatLiteral :: (HasStandardData builtin) => Provenance -> Int -> Expr var builtin
pattern NatLiteral p n <- Builtin p (getBuiltinConstructor -> Just (LNat n))
  where
    NatLiteral p n = Builtin p (mkBuiltinConstructor (LNat n))

pattern IntLiteral :: (HasStandardData builtin) => Provenance -> Int -> Expr var builtin
pattern IntLiteral p n <- Builtin p (getBuiltinConstructor -> Just (LInt n))
  where
    IntLiteral p n = Builtin p (mkBuiltinConstructor (LInt n))

pattern RatLiteral :: (HasStandardData builtin) => Provenance -> Rational -> Expr var builtin
pattern RatLiteral p n <- Builtin p (getBuiltinConstructor -> Just (LRat n))
  where
    RatLiteral p n = Builtin p (mkBuiltinConstructor (LRat n))

pattern VecLiteral ::
  (HasStandardData builtin) =>
  Provenance ->
  Expr var builtin ->
  [Arg var builtin] ->
  Expr var builtin
pattern VecLiteral p tElem xs <-
  BuiltinExpr p (getBuiltinConstructor -> Just (LVec _)) (RelevantImplicitArg _ tElem :| xs)
  where
    VecLiteral p tElem xs =
      BuiltinExpr p (mkBuiltinConstructor (LVec (length xs))) (RelevantImplicitArg p tElem :| xs)

pattern NilExpr ::
  (HasStandardData builtin) =>
  Provenance ->
  Type var builtin ->
  Expr var builtin
pattern NilExpr p tElem <- BuiltinExpr p (getBuiltinConstructor -> Just Nil) [RelevantImplicitArg _ tElem]
  where
    NilExpr p tElem = BuiltinExpr p (mkBuiltinConstructor Nil) [RelevantImplicitArg p tElem]

pattern ConsExpr ::
  (HasStandardData builtin) =>
  Provenance ->
  Type var builtin ->
  [Arg var builtin] ->
  Expr var builtin
pattern ConsExpr p tElem explicitArgs <-
  BuiltinExpr
    p
    (getBuiltinConstructor -> Just Cons)
    ( RelevantImplicitArg _ tElem
        :| explicitArgs
      )
  where
    ConsExpr p tElem explicitArgs =
      BuiltinExpr
        p
        (mkBuiltinConstructor Cons)
        ( RelevantImplicitArg p tElem
            :| explicitArgs
        )

mkList ::
  (HasStandardData builtin) =>
  Provenance ->
  Expr var builtin ->
  [Expr var builtin] ->
  Expr var builtin
mkList p elemType = foldr mkCons mkNil
  where
    mkNil = NilExpr p elemType
    mkCons x xs =
      ConsExpr
        p
        elemType
        [ RelevantExplicitArg p x,
          RelevantExplicitArg p xs
        ]

--------------------------------------------------------------------------------
-- Value constructors patterns

pattern VUnitLiteral :: (HasStandardData builtin) => Value builtin
pattern VUnitLiteral <- VBuiltin (getBuiltinConstructor -> Just LUnit) []
  where
    VUnitLiteral = VBuiltin (mkBuiltinConstructor LUnit) []

pattern VBoolLiteral :: (HasStandardData builtin) => Bool -> Value builtin
pattern VBoolLiteral x <- VBuiltin (getBuiltinConstructor -> Just (LBool x)) []
  where
    VBoolLiteral x = VBuiltin (mkBuiltinConstructor (LBool x)) []

pattern VIndexLiteral :: (HasStandardData builtin) => Int -> Value builtin
pattern VIndexLiteral x <- VBuiltin (getBuiltinConstructor -> Just (LIndex x)) []
  where
    VIndexLiteral x = VBuiltin (mkBuiltinConstructor (LIndex x)) []

pattern VNatLiteral :: (HasStandardData builtin) => Int -> Value builtin
pattern VNatLiteral x <- VBuiltin (getBuiltinConstructor -> Just (LNat x)) []
  where
    VNatLiteral x = VBuiltin (mkBuiltinConstructor (LNat x)) []

pattern VIntLiteral :: (HasStandardData builtin) => Int -> Value builtin
pattern VIntLiteral x <- VBuiltin (getBuiltinConstructor -> Just (LInt x)) []
  where
    VIntLiteral x = VBuiltin (mkBuiltinConstructor (LInt x)) []

pattern VRatLiteral :: (HasStandardData builtin) => Rational -> Value builtin
pattern VRatLiteral x <- VBuiltin (getBuiltinConstructor -> Just (LRat x)) []
  where
    VRatLiteral x = VBuiltin (mkBuiltinConstructor (LRat x)) []

pattern VNil :: (HasStandardData builtin) => Value builtin
pattern VNil <- VBuiltin (getBuiltinConstructor -> Just Nil) _

-- TODO should definitely be `isRelevant` not `isExplicit`
pattern VCons :: (HasStandardData builtin) => [VArg builtin] -> Value builtin
pattern VCons xs <- VBuiltin (getBuiltinConstructor -> Just Cons) (filter isExplicit -> xs)

-- TODO should definitely be `isRelevant` not `isExplicit`
pattern VVecLiteral :: (HasStandardData builtin) => [VArg builtin] -> Value builtin
pattern VVecLiteral xs <- VBuiltin (getBuiltinConstructor -> Just (LVec _)) (filter isExplicit -> xs)

mkVList :: (HasStandardData builtin) => [Value builtin] -> Value builtin
mkVList = foldr mkCons mkNil
  where
    mkNil = VBuiltin (mkBuiltinConstructor Nil) []
    mkCons y ys = VBuiltin (mkBuiltinConstructor Cons) (RelevantExplicitArg mempty <$> [y, ys])

mkVLVec :: (HasStandardData builtin) => [Value builtin] -> Value builtin
mkVLVec xs = VBuiltin (mkBuiltinConstructor (LVec (length xs))) (RelevantImplicitArg mempty VUnitLiteral : (RelevantExplicitArg mempty <$> xs))

getNatLiteral :: (HasStandardData builtin) => Value builtin -> Maybe Int
getNatLiteral = \case
  VNatLiteral d -> Just d
  _ -> Nothing

--------------------------------------------------------------------------------
-- Functions DSL

builtinFunction :: (HasStandardData builtin) => BuiltinFunction -> DSLExpr builtin
builtinFunction = builtin . mkBuiltinFunction

addNat :: (HasStandardData builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
addNat x y = builtinFunction (Add AddNat) @@ [x, y]

ite ::
  (HasStandardData builtin) =>
  DSLExpr builtin ->
  DSLExpr builtin ->
  DSLExpr builtin ->
  DSLExpr builtin ->
  DSLExpr builtin
ite t c e1 e2 = builtinFunction If @@@ [t] @@ [c, e1, e2]

--------------------------------------------------------------------------------
-- Value Function patterns

pattern VBuiltinFunction :: (HasStandardData builtin) => BuiltinFunction -> Spine builtin -> Value builtin
pattern VBuiltinFunction f args <- VBuiltin (getBuiltinFunction -> Just f) args
  where
    VBuiltinFunction f args = VBuiltin (mkBuiltinFunction f) args
