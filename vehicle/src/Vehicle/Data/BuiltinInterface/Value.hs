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

-- Can't use `[]` in a bidrectional pattern synonym until GHC 9.4.3??
pattern VRawBuiltinType :: (HasStandardTypes builtin) => BuiltinType -> WHNFType builtin
pattern VRawBuiltinType t <- VBuiltinType t []
  where
    VRawBuiltinType t = VBuiltinType t []

pattern VUnitType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VUnitType = VRawBuiltinType Unit

pattern VBoolType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VBoolType = VRawBuiltinType Bool

pattern VIndexType :: (HasStandardTypes builtin) => WHNFType builtin -> WHNFType builtin
pattern VIndexType size <- VBuiltinType Index [IrrelevantExplicitArg _ size]

pattern VNatType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VNatType = VRawBuiltinType Nat

pattern VIntType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VIntType = VRawBuiltinType Int

pattern VRatType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VRatType = VRawBuiltinType Rat

pattern VRawListType :: (HasStandardTypes builtin) => WHNFType builtin
pattern VRawListType = VRawBuiltinType List

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

pattern VConstructor :: (HasStandardData builtin) => BuiltinConstructor -> Spine strategy builtin -> Value strategy builtin
pattern VConstructor c spine <- VBuiltin (getBuiltinConstructor -> Just c) spine
  where
    VConstructor c spine = VBuiltin (mkBuiltinConstructor c) spine

pattern VUnitLiteral :: (HasStandardData builtin) => Value strategy builtin
pattern VUnitLiteral = VConstructor LUnit []

pattern VBoolLiteral :: (HasStandardData builtin) => Bool -> Value strategy builtin
pattern VBoolLiteral x = VConstructor (LBool x) []

pattern VIndexLiteral :: (HasStandardData builtin) => Int -> Value strategy builtin
pattern VIndexLiteral x = VConstructor (LIndex x) []

pattern VNatLiteral :: (HasStandardData builtin) => Int -> Value strategy builtin
pattern VNatLiteral x = VConstructor (LNat x) []

pattern VIntLiteral :: (HasStandardData builtin) => Int -> Value strategy builtin
pattern VIntLiteral x = VConstructor (LInt x) []

pattern VRatLiteral :: (HasStandardData builtin) => Rational -> Value strategy builtin
pattern VRatLiteral x = VConstructor (LRat x) []

pattern VNil :: (HasStandardData builtin) => Value strategy builtin
pattern VNil <- VConstructor Nil _

-- TODO should definitely be `isRelevant` not `isExplicit`
pattern VCons :: (HasStandardData builtin) => VArg strategy builtin -> VArg strategy builtin -> Value strategy builtin
pattern VCons x xs <- VConstructor Cons (filter isExplicit -> [x, xs])

-- TODO should definitely be `isRelevant` not `isExplicit`
pattern VVecLiteral :: (HasStandardData builtin) => [VArg strategy builtin] -> Value strategy builtin
pattern VVecLiteral xs <- VConstructor (LVec _) (filter isExplicit -> xs)

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
pattern VOrder dom op x y <- VBuiltinFunction (Order dom op) (reverse -> (argExpr -> y) : (argExpr -> x) : _)

pattern VOrderRat :: (HasStandardData builtin) => OrderOp -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VOrderRat op x y = VOp2 (Order OrderRat op) x y

pattern VEqualOp :: (HasStandardData builtin) => EqualityDomain -> EqualityOp -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VEqualOp dom op x y <- VBuiltinFunction (Equals dom op) (reverse -> (argExpr -> y) : (argExpr -> x) : _)

pattern VEqual :: (HasStandardData builtin) => EqualityDomain -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VEqual dom x y <- VEqualOp dom Eq x y

pattern VEqualRat :: (HasStandardData builtin) => WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VEqualRat x y = VOp2 (Equals EqRat Eq) x y

pattern VNotEqual :: (HasStandardData builtin) => EqualityDomain -> WHNFValue builtin -> WHNFValue builtin -> WHNFValue builtin
pattern VNotEqual dom x y <- VEqualOp dom Neq x y

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
