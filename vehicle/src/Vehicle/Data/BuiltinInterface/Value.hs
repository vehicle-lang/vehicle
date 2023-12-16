module Vehicle.Data.BuiltinInterface.Value where

import Vehicle.Data.BuiltinInterface
import Vehicle.Data.NormalisedExpr
import Vehicle.Libraries.StandardLibrary.Definitions
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Type values

pattern VBuiltinType :: (HasStandardTypes builtin) => BuiltinType -> WHNFSpine builtin -> WHNFType builtin
pattern VBuiltinType c args <- VBuiltin (getBuiltinType -> Just c) args
  where
    VBuiltinType c args = VBuiltin (mkBuiltinType c) args

pattern VUnitType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VUnitType = VBuiltinType Unit []

pattern VBoolType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VBoolType = VBuiltinType Bool []

pattern VIndexType :: (HasStandardTypes builtin) => WHNFType builtin -> WHNFType builtin
pattern VIndexType size <- VBuiltinType Index [IrrelevantExplicitArg _ size]

pattern VNatType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VNatType = VBuiltinType Nat []

pattern VIntType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VIntType = VBuiltinType Int []

pattern VRatType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VRatType = VBuiltinType Rat []

pattern VRawListType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VRawListType = VBuiltinType List []

pattern VListType :: (HasStandardTypes builtin) => WHNFType builtin -> WHNFType builtin
pattern VListType tElem <- VBuiltinType List [RelevantExplicitArg _ tElem]

pattern VVectorType :: (HasStandardTypes builtin) => WHNFType builtin -> WHNFValue builtin -> WHNFType builtin
pattern VVectorType tElem dim <- VBuiltinType Vector [RelevantExplicitArg _ tElem, IrrelevantExplicitArg _ dim]

pattern VTensorType :: (HasStandardTypes builtin) => WHNFType builtin -> WHNFValue builtin -> WHNFType builtin
pattern VTensorType tElem dims <-
  VFreeVar TensorIdent [RelevantExplicitArg _ tElem, RelevantExplicitArg _ dims]

mkVVectorType :: (HasStandardTypes builtin) => WHNFType builtin -> WHNFValue builtin -> WHNFType builtin
mkVVectorType tElem dim =
  VBuiltinType
    Vector
    [ Arg mempty Explicit Relevant tElem,
      Arg mempty Explicit Irrelevant dim
    ]

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
  where
    VNil = VBuiltin (mkBuiltinConstructor Nil) []

-- TODO should definitely be `isRelevant` not `isExplicit`
pattern VCons :: (HasStandardData builtin) => VArg strategy builtin -> VArg strategy builtin -> Value strategy builtin
pattern VCons x xs <- VBuiltin (getBuiltinConstructor -> Just Cons) (filter isExplicit -> [x, xs])

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
-- Standard library patterns

pattern VStandardLib :: StdLibFunction -> WHNFSpine builtin -> WHNFValue builtin
pattern VStandardLib fn spine <- VFreeVar (findStdLibFunction -> Just fn) spine
  where
    VStandardLib fn spine = VFreeVar (identifierOf fn) spine

--------------------------------------------------------------------------------
-- WHNFValue Function patterns

pattern VBuiltinFunction :: (HasStandardData builtin) => BuiltinFunction -> WHNFSpine builtin -> WHNFValue builtin
pattern VBuiltinFunction f args <- VBuiltin (getBuiltinFunction -> Just f) args
  where
    VBuiltinFunction f args = VBuiltin (mkBuiltinFunction f) args

pattern VOp1 :: (HasStandardData builtin) => BuiltinFunction -> WHNFValue builtin -> WHNFValue builtin
pattern VOp1 op x <- VBuiltinFunction op [RelevantExplicitArg _ x]
  where
    VOp1 op x = VBuiltinFunction op [Arg mempty Explicit Relevant x]

pattern VOp2 :: (HasStandardData builtin) => BuiltinFunction -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VOp2 op x y <- VBuiltinFunction op [RelevantExplicitArg _ x, RelevantExplicitArg _ y]
  where
    VOp2 op x y = VBuiltinFunction op [Arg mempty Explicit Relevant x, Arg mempty Explicit Relevant y]

pattern VAnd :: (HasStandardData builtin) => WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VAnd x y = VOp2 And x y

pattern VOr :: (HasStandardData builtin) => WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VOr x y = VOp2 Or x y

pattern VNot :: (HasStandardData builtin) => WHNFValue builtin -> WHNFValue builtin
pattern VNot x = VOp1 Not x

pattern VIf :: (HasStandardData builtin) => WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VIf t c x y <- VBuiltinFunction If [RelevantImplicitArg _ t, RelevantExplicitArg _ c, RelevantExplicitArg _ x, RelevantExplicitArg _ y]
  where
    VIf t c x y = VBuiltinFunction If [Arg mempty (Implicit True) Relevant t, Arg mempty Explicit Relevant c, Arg mempty Explicit Relevant x, Arg mempty Explicit Relevant y]

pattern VOrder :: (HasStandardData builtin) => OrderDomain -> OrderOp -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VOrder dom op x y = VOp2 (Order dom op) x y

pattern VEqualOp :: (HasStandardData builtin) => EqualityDomain -> EqualityOp -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VEqualOp dom op x y = VOp2 (Equals dom op) x y

pattern VEqual :: (HasStandardData builtin) => EqualityDomain -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VEqual dom x y = VEqualOp dom Eq x y

pattern VNotEqual :: (HasStandardData builtin) => EqualityDomain -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VNotEqual dom x y = VEqualOp dom Neq x y

pattern VNeg :: (HasStandardData builtin) => NegDomain -> WHNFValue builtin -> WHNFValue builtin
pattern VNeg dom x = VOp1 (Neg dom) x

pattern VAdd :: (HasStandardData builtin) => AddDomain -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VAdd dom x y = VOp2 (Add dom) x y

pattern VSub :: (HasStandardData builtin) => SubDomain -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VSub dom x y = VOp2 (Sub dom) x y

pattern VMul :: (HasStandardData builtin) => MulDomain -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VMul dom x y = VOp2 (Mul dom) x y

pattern VDiv :: (HasStandardData builtin) => DivDomain -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VDiv dom x y = VOp2 (Div dom) x y
