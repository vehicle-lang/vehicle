module Vehicle.Data.BuiltinInterface where

import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Data.DSL
import Vehicle.Data.DeBruijn
import Vehicle.Data.NormalisedExpr
import Vehicle.Libraries.StandardLibrary.Definitions
import Vehicle.Prelude
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Interface to standard builtins
--------------------------------------------------------------------------------

-- At various points in the compiler, we have different sets of builtins (e.g.
-- first time we type-check we use the standard set of builtins + type +
-- type classes, but when checking polarity and linearity information we
-- subsitute out all the types and type-classes for new types.)
--
-- The interfaces defined in this file allow us to abstract over the exact set
-- of builtins being used, and therefore allows us to define operations
-- (e.g. normalisation) once, rather than once for each builtin type.

--------------------------------------------------------------------------------
-- HasStandardData

-- | Indicates that this set of builtins has the standard builtin constructors
-- and functions.
class HasStandardData builtin where
  mkBuiltinConstructor :: BuiltinConstructor -> builtin
  getBuiltinConstructor :: builtin -> Maybe BuiltinConstructor

  mkBuiltinFunction :: BuiltinFunction -> builtin
  getBuiltinFunction :: builtin -> Maybe BuiltinFunction

  isTypeClassOp :: builtin -> Bool

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
-- HasStandardTypes

-- | Indicates that this set of builtins has the standard set of types.
class HasStandardTypes builtin where
  mkBuiltinType :: BuiltinType -> builtin
  getBuiltinType :: builtin -> Maybe BuiltinType

  mkNatInDomainConstraint :: builtin

instance HasStandardTypes Builtin where
  mkBuiltinType = BuiltinType
  getBuiltinType = \case
    BuiltinType c -> Just c
    _ -> Nothing

  mkNatInDomainConstraint = NatInDomainConstraint

--------------------------------------------------------------------------------
-- HasStandardBuiltins

-- | Indicates that this set of builtins has the standard set of constructors,
-- functions and types.
class HasStandardTypeClasses builtin where
  mkBuiltinTypeClass :: TypeClass -> builtin

instance HasStandardTypeClasses Builtin where
  mkBuiltinTypeClass = TypeClass

--------------------------------------------------------------------------------
-- HasStandardBuiltins

-- | Indicates that this set of builtins has the standard set of constructors,
-- functions and types.
type HasStandardBuiltins builtin =
  ( HasStandardTypes builtin,
    HasStandardData builtin
  )

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

-- | Use to convert builtins for printing that have no representation in the
-- standard `Builtin` type.
cheatConvertBuiltin :: (Pretty builtin) => Provenance -> builtin -> Expr var Builtin
cheatConvertBuiltin p b = FreeVar p $ Identifier StdLib (layoutAsText $ pretty b)

--------------------------------------------------------------------------------
-- Typable builtin

class (PrintableBuiltin builtin) => TypableBuiltin builtin where
  -- | Construct a type for the builtin
  typeBuiltin ::
    Provenance -> builtin -> Type Ix builtin

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

natInDomainConstraint :: (HasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
natInDomainConstraint n t = builtin mkNatInDomainConstraint @@ [n, t]

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

--------------------------------------------------------------------------------
-- Type values

pattern VBuiltinType :: (HasStandardTypes builtin) => BuiltinType -> WHNFSpine builtin -> WHNFType builtin
pattern VBuiltinType c args <- VBuiltin (getBuiltinType -> Just c) args
  where
    VBuiltinType c args = VBuiltin (mkBuiltinType c) args

pattern VBoolType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VBoolType <- VBuiltinType Bool []
  where
    VBoolType = VBuiltinType Bool []

pattern VIndexType :: (HasStandardTypes builtin) => WHNFType builtin -> WHNFType builtin
pattern VIndexType size <- VBuiltinType Index [IrrelevantExplicitArg _ size]

pattern VNatType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VNatType <- VBuiltinType Nat []
  where
    VNatType = VBuiltinType Nat []

pattern VIntType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VIntType <- VBuiltinType Int []
  where
    VIntType = VBuiltinType Int []

pattern VRatType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VRatType <- VBuiltinType Rat []
  where
    VRatType = VBuiltinType Rat []

pattern VRawListType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VRawListType <- VBuiltinType List []
  where
    VRawListType = VBuiltinType List []

pattern VListType :: (HasStandardTypes builtin) => WHNFType builtin -> WHNFType builtin
pattern VListType tElem <- VBuiltinType List [RelevantExplicitArg _ tElem]

pattern VVectorType :: (HasStandardTypes builtin) => WHNFType builtin -> WHNFValue builtin -> WHNFType builtin
pattern VVectorType tElem dim <- VBuiltinType Vector [RelevantExplicitArg _ tElem, IrrelevantExplicitArg _ dim]

pattern VTensorType :: (HasStandardTypes builtin) => WHNFType builtin -> WHNFValue builtin -> WHNFType builtin
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

pattern AnnExpr :: (HasStandardData builtin) => Provenance -> Type var builtin -> Expr var builtin -> Expr var builtin
pattern AnnExpr p t e <- BuiltinExpr p (getBuiltinFunction -> Just Ann) [ExplicitArg _ _ t, ExplicitArg _ _ e]

pattern VecLiteral ::
  (HasStandardData builtin) =>
  Provenance ->
  Expr var builtin ->
  [Arg var builtin] ->
  Expr var builtin
pattern VecLiteral p tElem xs <-
  BuiltinExpr p (getBuiltinConstructor -> Just (LVec _)) (RelevantImplicitArg _ tElem :| xs)

mkList ::
  forall var builtin.
  (HasStandardData builtin) =>
  Provenance ->
  Expr var builtin ->
  [Expr var builtin] ->
  Expr var builtin
mkList p elemType = foldr mkCons mkNil
  where
    mkNil :: Expr var builtin
    mkNil = BuiltinExpr p (mkBuiltinConstructor Nil) [Arg p (Implicit True) Relevant elemType]

    mkCons ::
      (HasStandardData builtin) =>
      Expr var builtin ->
      Expr var builtin ->
      Expr var builtin
    mkCons x xs =
      BuiltinExpr
        p
        (mkBuiltinConstructor Cons)
        ( Arg p (Implicit True) Relevant elemType
            :| [ Arg p Explicit Relevant x,
                 Arg p Explicit Relevant xs
               ]
        )

--------------------------------------------------------------------------------
-- WHNFValue constructors patterns

pattern VUnitLiteral :: (HasStandardData builtin) => Value strategy builtin
pattern VUnitLiteral <- VBuiltin (getBuiltinConstructor -> Just LUnit) []
  where
    VUnitLiteral = VBuiltin (mkBuiltinConstructor LUnit) []

pattern VBoolLiteral :: (HasStandardData builtin) => Bool -> Value strategy builtin
pattern VBoolLiteral x <- VBuiltin (getBuiltinConstructor -> Just (LBool x)) []
  where
    VBoolLiteral x = VBuiltin (mkBuiltinConstructor (LBool x)) []

pattern VIndexLiteral :: (HasStandardData builtin) => Int -> Value strategy builtin
pattern VIndexLiteral x <- VBuiltin (getBuiltinConstructor -> Just (LIndex x)) []
  where
    VIndexLiteral x = VBuiltin (mkBuiltinConstructor (LIndex x)) []

pattern VNatLiteral :: (HasStandardData builtin) => Int -> Value strategy builtin
pattern VNatLiteral x <- VBuiltin (getBuiltinConstructor -> Just (LNat x)) []
  where
    VNatLiteral x = VBuiltin (mkBuiltinConstructor (LNat x)) []

pattern VIntLiteral :: (HasStandardData builtin) => Int -> Value strategy builtin
pattern VIntLiteral x <- VBuiltin (getBuiltinConstructor -> Just (LInt x)) []
  where
    VIntLiteral x = VBuiltin (mkBuiltinConstructor (LInt x)) []

pattern VRatLiteral :: (HasStandardData builtin) => Rational -> Value strategy builtin
pattern VRatLiteral x <- VBuiltin (getBuiltinConstructor -> Just (LRat x)) []
  where
    VRatLiteral x = VBuiltin (mkBuiltinConstructor (LRat x)) []

pattern VNil :: (HasStandardData builtin) => Value strategy builtin
pattern VNil <- VBuiltin (getBuiltinConstructor -> Just Nil) _

-- TODO should definitely be `isRelevant` not `isExplicit`
pattern VCons :: (HasStandardData builtin) => [VArg strategy builtin] -> Value strategy builtin
pattern VCons xs <- VBuiltin (getBuiltinConstructor -> Just Cons) (filter isExplicit -> xs)

-- TODO should definitely be `isRelevant` not `isExplicit`
pattern VVecLiteral :: (HasStandardData builtin) => [VArg strategy builtin] -> Value strategy builtin
pattern VVecLiteral xs <- VBuiltin (getBuiltinConstructor -> Just (LVec _)) (filter isExplicit -> xs)

mkVList :: (HasStandardData builtin) => [Value strategy builtin] -> Value strategy builtin
mkVList = foldr mkCons mkNil
  where
    mkNil = VBuiltin (mkBuiltinConstructor Nil) []
    mkCons y ys = VBuiltin (mkBuiltinConstructor Cons) (Arg mempty Explicit Relevant <$> [y, ys])

mkVLVec :: (HasStandardData builtin) => [Value strategy builtin] -> Value strategy builtin
mkVLVec xs =
  VBuiltin
    (mkBuiltinConstructor (LVec (length xs)))
    (Arg mempty (Implicit True) Relevant VUnitLiteral : (Arg mempty Explicit Relevant <$> xs))

getNatLiteral :: (HasStandardData builtin) => Value strategy builtin -> Maybe Int
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
-- WHNFValue Function patterns

pattern VBuiltinFunction :: (HasStandardData builtin) => BuiltinFunction -> WHNFSpine builtin -> WHNFValue builtin
pattern VBuiltinFunction f args <- VBuiltin (getBuiltinFunction -> Just f) args
  where
    VBuiltinFunction f args = VBuiltin (mkBuiltinFunction f) args

--------------------------------------------------------------------------------
-- Type classes

builtinTypeClass :: (HasStandardTypeClasses builtin) => TypeClass -> DSLExpr builtin
builtinTypeClass = builtin . mkBuiltinTypeClass

typeClass :: (HasStandardTypeClasses builtin) => TypeClass -> NonEmpty (DSLExpr builtin) -> DSLExpr builtin
typeClass tc args = builtinTypeClass tc @@ args

hasEq :: (HasStandardTypeClasses builtin) => EqualityOp -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasEq eq t1 t2 = typeClass (HasEq eq) [t1, t2]

hasOrd :: (HasStandardTypeClasses builtin) => OrderOp -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasOrd ord t1 t2 = typeClass (HasOrd ord) [t1, t2]

hasQuantifier :: (HasStandardTypeClasses builtin) => Quantifier -> DSLExpr builtin -> DSLExpr builtin
hasQuantifier q t = typeClass (HasQuantifier q) [t]

numOp2TypeClass :: (HasStandardTypeClasses builtin) => TypeClass -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
numOp2TypeClass tc t1 t2 t3 = typeClass tc [t1, t2, t3]

hasAdd :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasAdd = numOp2TypeClass HasAdd

hasSub :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasSub = numOp2TypeClass HasSub

hasMul :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasMul = numOp2TypeClass HasMul

hasDiv :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasDiv = numOp2TypeClass HasDiv

hasNeg :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasNeg t1 t2 = typeClass HasNeg [t1, t2]

hasMap :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
hasMap tCont = typeClass HasMap [tCont]

hasFold :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
hasFold tCont = typeClass HasFold [tCont]

hasQuantifierIn :: (HasStandardTypeClasses builtin) => Quantifier -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasQuantifierIn q tCont tElem tRes = typeClass (HasQuantifierIn q) [tCont, tElem, tRes]

hasNatLits :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
hasNatLits t = typeClass HasNatLits [t]

hasRatLits :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
hasRatLits t = typeClass HasRatLits [t]

hasVecLits :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasVecLits n d = typeClass HasVecLits [n, d]