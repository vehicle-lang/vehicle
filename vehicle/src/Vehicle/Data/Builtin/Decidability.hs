module Vehicle.Data.Builtin.Decidability
  ( module Vehicle.Data.Builtin.Decidability,
    module Vehicle.Data.Builtin.Core.BasicOperations,
  )
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Prelude (Expr (..), normAppList)
import Vehicle.Data.Builtin.Core.BasicOperations
import Vehicle.Data.Builtin.Core.Derived (DerivedFunction (..))
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Standard (Builtin, BuiltinConstructor (..), BuiltinFunction (..), BuiltinType (..))
import Vehicle.Data.Code.DSL (tDim, tDims)
import Vehicle.Data.Code.Interface
import Vehicle.Data.DSL
import Vehicle.Data.Tensor (BoolTensor, anyTensor)
import Vehicle.Prelude (Pretty (..), Relevance (..), Visibility (..), developerError, explicit, (<+>))

--------------------------------------------------------------------------------
-- Data

data TensorTypeClassField
  = FieldFromBoolTensorLiteral
  | FieldNot
  | FieldAnd
  | FieldOr
  | FieldImplies
  | FieldReduceAnd
  | FieldReduceOr
  | FieldCompareNat ComparisonOp
  | FieldCompareIndex ComparisonOp
  | FieldCompareRatTensor ComparisonOp
  | FieldQuantifyIndex Quantifier
  | FieldQuantifyInList Quantifier
  | FieldForeachTensor
  | FieldAtTensor
  deriving (Eq, Ord, Show, Generic)

instance Hashable TensorTypeClassField

data VectorTypeClassField
  = FieldFromVectorLiteral
  | FieldForeachVector
  | FieldAtVector
  deriving (Eq, Ord, Show, Generic)

instance Hashable VectorTypeClassField

data DecidabilityBuiltinTypeClass
  = IsTensorType
  | IsVectorType
  | HasTensorTypeClassField TensorTypeClassField
  | HasVectorTypeClassField VectorTypeClassField
  | ValidPropertyType
  deriving (Eq, Ord, Show, Generic)

instance Hashable DecidabilityBuiltinTypeClass

data DecidabilityBuiltinTypeClassOp
  = TensorTypeTC
  | VectorTypeTC
  | TensorTypeClassFieldTC TensorTypeClassField
  | VectorTypeClassFieldTC VectorTypeClassField
  deriving (Eq, Ord, Show, Generic)

instance Hashable DecidabilityBuiltinTypeClassOp

-- | Constructors for types in the language. The types and type-classes
-- are viewed as constructors for `Type`.
data DecidabilityBuiltinFunction
  = PropType
  | BoolTensorToProp
  | BoolVectorToProp
  | PropTrue
  | PropFalse
  | PropNot
  | PropAnd
  | PropOr
  | PropImplies
  | PropCompareNat ComparisonOp
  | PropCompareIndex ComparisonOp
  | PropCompareRatTensor ComparisonOp
  | PropNaryProduct
  | PropNaryProductAt
  | PropNaryProductForeach
  | -- Taken from DerivedFunctions
    PropQuantifyIndex Quantifier
  | PropQuantifyInList Quantifier
  deriving (Eq, Ord, Show, Generic)

instance Hashable DecidabilityBuiltinFunction

newtype DecidabilityBuiltinConstructor
  = DecBoolTensor BoolTensor
  deriving (Eq, Show, Ord, Generic)

instance Hashable DecidabilityBuiltinConstructor

-- | The builtin types after translation to loss functions (missing all builtins
-- that involve the Bool type).
data DecidabilityBuiltin
  = StandardBuiltinType BuiltinType
  | StandardBuiltinFunction BuiltinFunction
  | StandardBuiltinConstructor BuiltinConstructor
  | StandardBuiltinDerivedFunction DerivedFunction
  | DecidabilityBuiltinTypeClass DecidabilityBuiltinTypeClass
  | DecidabilityBuiltinTypeClassOp DecidabilityBuiltinTypeClassOp
  | DecidabilityBuiltinFunction DecidabilityBuiltinFunction
  deriving (Show, Ord, Eq, Generic)

instance Hashable DecidabilityBuiltin

--------------------------------------------------------------------------------
-- Accessors

functionAccessor :: BuiltinFunction -> Accessor DecidabilityBuiltin ()
functionAccessor b =
  Access
    { getExpr = \case
        StandardBuiltinFunction b1 | b == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> StandardBuiltinFunction b
    }

constructorAccessor :: BuiltinConstructor -> Accessor DecidabilityBuiltin ()
constructorAccessor b =
  Access
    { getExpr = \case
        StandardBuiltinConstructor b1 | b == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> StandardBuiltinConstructor b
    }

typeAccessor :: BuiltinType -> Accessor DecidabilityBuiltin ()
typeAccessor b =
  Access
    { getExpr = \case
        StandardBuiltinType b1 | b == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> StandardBuiltinType b
    }

instance BuiltinHasStandardTypes DecidabilityBuiltin where
  accessBuiltinType =
    Access
      { mkExpr = StandardBuiltinType,
        getExpr = \case
          StandardBuiltinType c -> Just c
          _ -> Nothing
      }

instance BuiltinHasVectors DecidabilityBuiltin where
  accessVecLitBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinConstructor VectorLiteral -> Just ()
          _ -> Nothing,
        mkExpr = \() -> StandardBuiltinConstructor VectorLiteral
      }

  accessAtVectorBuiltin = functionAccessor AtVector

instance BuiltinHasStandardData DecidabilityBuiltin where
  accessBuiltinFunction =
    Access
      { mkExpr = StandardBuiltinFunction,
        getExpr = \case
          StandardBuiltinFunction c -> Just c
          _ -> Nothing
      }

  accessBuiltinConstructor =
    Access
      { mkExpr = StandardBuiltinConstructor,
        getExpr = \case
          StandardBuiltinConstructor c -> Just c
          _ -> Nothing
      }

instance BuiltinHasNatType DecidabilityBuiltin where
  accessNatTypeBuiltin = typeAccessor NatType

instance BuiltinHasNatLiterals DecidabilityBuiltin where
  accessNatLitBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinConstructor (NatLiteral n) -> Just n
          _ -> Nothing,
        mkExpr = StandardBuiltinConstructor . NatLiteral
      }

  accessNatTensorLitBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinConstructor (NatTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = StandardBuiltinConstructor . NatTensorLiteral
      }

  accessAddNatBuiltin = functionAccessor (Add AddNat)
  accessMulNatBuiltin = functionAccessor (Mul MulNat)

instance BuiltinHasListLiterals DecidabilityBuiltin where
  accessNilBuiltin = constructorAccessor Nil
  accessConsBuiltin = constructorAccessor Cons
  accessMapListBuiltin = functionAccessor MapList
  accessFoldListBuiltin = functionAccessor FoldList
  accessReverseListBuiltin = functionAccessor ReverseList
  accessAppendListBuiltin = functionAccessor AppendList

instance BuiltinHasIterate DecidabilityBuiltin where
  accessIterateBuiltin = functionAccessor Iterate

--------------------------------------------------------------------------------
-- Pretty printing

instance Pretty DecidabilityBuiltinTypeClass where
  pretty t = case t of
    HasTensorTypeClassField {} -> pretty $ show t
    HasVectorTypeClassField {} -> pretty $ show t
    IsTensorType -> pretty $ show t
    IsVectorType -> pretty $ show t
    ValidPropertyType -> pretty $ show t

instance Pretty DecidabilityBuiltinFunction where
  pretty = \case
    PropType -> "Prop"
    BoolTensorToProp -> "boolTensorToProp"
    BoolVectorToProp -> "boolVectorToProp"
    PropTrue -> "true" <> symbol
    PropFalse -> "false" <> symbol
    PropNot -> pretty Not <> symbol
    PropAnd -> pretty And <> symbol
    PropOr -> pretty Or <> symbol
    PropImplies -> pretty Implies <> symbol
    PropCompareNat op -> pretty (CompareNat op) <> symbol
    PropCompareIndex op -> pretty (CompareIndex op) <> symbol
    PropCompareRatTensor op -> pretty (CompareRatTensor op) <> symbol
    PropQuantifyIndex q -> pretty (QuantifyIndex q) <> symbol
    PropQuantifyInList q -> pretty (QuantifyInList q) <> symbol
    PropNaryProduct -> pretty VectorType <> symbol
    PropNaryProductForeach -> pretty ForeachVector <> symbol
    PropNaryProductAt -> pretty AtVector <> symbol
    where
      symbol = "ᵖ"

instance Pretty DecidabilityBuiltinConstructor where
  pretty = \case
    DecBoolTensor t -> pretty t

instance Pretty DecidabilityBuiltinTypeClassOp where
  pretty t = case t of
    TensorTypeTC -> pretty $ show t
    VectorTypeTC -> pretty $ show t
    TensorTypeClassFieldTC {} -> pretty $ show t
    VectorTypeClassFieldTC {} -> pretty $ show t

instance Pretty DecidabilityBuiltin where
  pretty = \case
    StandardBuiltinType t -> pretty t
    StandardBuiltinFunction f -> pretty f
    StandardBuiltinConstructor c -> pretty c
    StandardBuiltinDerivedFunction f -> pretty f
    DecidabilityBuiltinTypeClass t -> pretty t
    DecidabilityBuiltinTypeClassOp t -> pretty t
    DecidabilityBuiltinFunction f -> pretty f

instance ConvertableBuiltin DecidabilityBuiltin Builtin where
  convertBuiltin p b = case b of
    StandardBuiltinType t -> convertBuiltin p t
    StandardBuiltinFunction f -> convertBuiltin p f
    StandardBuiltinConstructor c -> convertBuiltin p c
    StandardBuiltinDerivedFunction f -> convertBuiltin p f
    DecidabilityBuiltinTypeClass t -> cheatConvertBuiltin p (pretty t)
    DecidabilityBuiltinTypeClassOp t -> cheatConvertBuiltin p (pretty t)
    DecidabilityBuiltinFunction f -> cheatConvertBuiltin p (pretty f)

instance PrintableBuiltin DecidabilityBuiltin where
  coercionArgs = const Nothing
  isDerivedBuiltin = const Nothing

--------------------------------------------------------------------------------
-- Normalisation

evalBoolTensorToProp ::
  forall m.
  (MonadNormBuiltin m) =>
  EvalSimple Expr Expr TensorOp1Args DecidabilityBuiltin m
evalBoolTensorToProp (TensorOp1Args _ value) = do
  forcedValue <- force value
  return $ case getExpr accessBuiltinC forcedValue of
    Just (StandardBuiltinConstructor (BoolTensorLiteral t), []) -> do
      let op = if anyTensor not t then PropFalse else PropTrue
      Evaluated $ mkExpr accessBuiltinC (DecidabilityBuiltinFunction op, [])
    _ -> developerError $ "Should not be possible to have non-literal" <+> pretty BoolTensorToProp <+> "args"

evalBoolVectorToProp ::
  (MonadNormBuiltin m) =>
  EvalSimple Expr Expr VectorOp1Args DecidabilityBuiltin m
evalBoolVectorToProp args = return $ case args of
  VectorOp1Args _ (IVecLiteral _ _ xs) -> case xs of
    [] -> Evaluated $ mkExpr accessBuiltinC (DecidabilityBuiltinFunction PropTrue, [])
    (v : vs) -> do
      let andFn a b = normAppList (Builtin mempty (DecidabilityBuiltinFunction PropAnd)) (explicit <$> [a, b])
      Evaluated $ foldr andFn v vs
  _ -> developerError $ "Should not be possible to have non-literal" <+> pretty BoolTensorToProp <+> "args"

instance NormalisableBuiltin DecidabilityBuiltin where
  evalScheme = \case
    StandardBuiltinFunction Iterate -> Eval evalIterate
    StandardBuiltinFunction ReverseList -> Eval evalReverseList
    StandardBuiltinFunction AppendList -> Eval evalAppendList
    DecidabilityBuiltinTypeClassOp {} -> TypeClassOperation
    _ -> None

  {-
    blockingStatus b spine = case b of
      StandardBuiltinFunction Iterate -> functionBlockingStatus Iterate spine
      StandardBuiltinFunction FoldList -> functionBlockingStatus FoldList spine
      StandardBuiltinFunction AppendList -> functionBlockingStatus AppendList spine
      _ -> DoesNotReduce
  -}

  isTypeClassOp = \case
    DecidabilityBuiltinTypeClassOp {} -> True
    _ -> False

  isCast p e = case e of
    DecidabilityBuiltinFunction BoolTensorToProp -> Just $ forceEvalSimpleBuiltin p e evalBoolTensorToProp
    DecidabilityBuiltinFunction BoolVectorToProp -> Just $ forceEvalSimpleBuiltin p e evalBoolVectorToProp
    _ -> Nothing

--------------------------------------------------------------------------------
-- DSL

isTensorType :: DSLExpr DecidabilityBuiltin
isTensorType = builtin (DecidabilityBuiltinTypeClass IsTensorType)

isVectorType :: DSLExpr DecidabilityBuiltin
isVectorType = builtin (DecidabilityBuiltinTypeClass IsVectorType)

decFunction :: DecidabilityBuiltinFunction -> DSLExpr DecidabilityBuiltin
decFunction f = builtin (DecidabilityBuiltinFunction f)

tProp :: DSLExpr DecidabilityBuiltin
tProp = decFunction PropType

propTensor :: DSLExpr DecidabilityBuiltin
propTensor =
  lam "t" Explicit Relevant type0 $
    const $
      lam "ds" Explicit Irrelevant tDims $
        const
          tProp

propVector :: DSLExpr DecidabilityBuiltin
propVector =
  lam "t" Explicit Relevant type0 $
    const $
      lam "d" Explicit Irrelevant tDim $
        const
          tProp

builtinDerivedFunction :: DerivedFunction -> DSLExpr DecidabilityBuiltin
builtinDerivedFunction = builtin . StandardBuiltinDerivedFunction

builtinDecidableTypeClass :: DecidabilityBuiltinTypeClass -> DSLExpr DecidabilityBuiltin
builtinDecidableTypeClass = builtin . DecidabilityBuiltinTypeClass
