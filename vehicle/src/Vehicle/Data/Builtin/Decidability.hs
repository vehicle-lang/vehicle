module Vehicle.Data.Builtin.Decidability
  ( module Vehicle.Data.Builtin.Decidability,
    module Vehicle.Syntax.Builtin.BasicOperations,
  )
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Prettyprinter (brackets)
import Vehicle.Compile.Normalise.NBE (NormalisableBuiltin)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Normalise (BlockingArgs (..), EvalScheme (..), MonadNormBuiltin, NormalisableBuiltin (..), evalFoldList, evalIterate, forceEvalSimpleBuiltin)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Standard (Builtin, BuiltinConstructor (..), BuiltinFunction (..), BuiltinType, DerivedFunction)
import Vehicle.Data.Code.DSL (tDims)
import Vehicle.Data.Code.Interface
import Vehicle.Data.DSL
import Vehicle.Data.Tensor (BoolTensor, anyTensor)
import Vehicle.Prelude (Pretty (..), Relevance (..), Visibility (..), braces, developerError, (<+>))
import Vehicle.Syntax.Builtin.BasicOperations
import Vehicle.Syntax.Builtin.Derived (DerivedFunction (..))
import Vehicle.Syntax.Sugar (BinderType (..))

--------------------------------------------------------------------------------
-- Data

data DecidabilityBuiltinTypeClass
  = IsBoolType
  | IsTensorType
  | HasBoolTensorLiterals
  | HasNot
  | HasAnd
  | HasOr
  | HasImplies
  | HasCompareNat ComparisonOp
  | HasCompareIndex ComparisonOp
  | HasCompareRatTensorPointwise ComparisonOp
  | HasReduceAndTensor
  | HasReduceOrTensor
  | HasQuantifyIndex Quantifier
  | HasQuantifyInList Quantifier
  | HasCompareRatTensorReduced ComparisonOp
  deriving (Eq, Ord, Show, Generic)

instance Hashable DecidabilityBuiltinTypeClass

data DecidabilityBuiltinTypeClassOp
  = BoolTypeTC
  | TensorTypeTC
  | FromBoolTensorLitTC
  | NotTC
  | AndTC
  | OrTC
  | ImpliesTC
  | CompareNatTC ComparisonOp
  | CompareIndexTC ComparisonOp
  | CompareRatTensorPointwiseTC ComparisonOp
  | ReduceAndTensorTC
  | ReduceOrTensorTC
  | QuantifyIndexTC Quantifier
  | QuantifyInListTC Quantifier
  | CompareRatTensorReducedTC ComparisonOp
  deriving (Eq, Ord, Show, Generic)

instance Hashable DecidabilityBuiltinTypeClassOp

-- | Constructors for types in the language. The types and type-classes
-- are viewed as constructors for `Type`.
data DecidabilityBuiltinFunction
  = BoolTensorToType
  | TypeTrue
  | TypeFalse
  | TypeNot
  | TypeAnd
  | TypeOr
  | TypeImplies
  | TypeCompareNat ComparisonOp
  | TypeCompareIndex ComparisonOp
  | TypeCompareRatTensorPointwise ComparisonOp
  | -- Taken from DerivedFunctions
    TypeQuantifyIndex Quantifier
  | TypeQuantifyInList Quantifier
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
  deriving (Show, Eq, Generic)

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

instance BuiltinHasStandardTypes DecidabilityBuiltin where
  accessBuiltinType =
    Access
      { mkExpr = StandardBuiltinType,
        getExpr = \case
          StandardBuiltinType c -> Just c
          _ -> Nothing
      }

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
  accessNilBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinConstructor Nil -> Just ()
          _ -> Nothing,
        mkExpr = \() -> StandardBuiltinConstructor Nil
      }

  accessConsBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinConstructor Cons -> Just ()
          _ -> Nothing,
        mkExpr = \() -> StandardBuiltinConstructor Cons
      }

  accessMapListBuiltin = functionAccessor MapList
  accessFoldListBuiltin = functionAccessor FoldList

instance BuiltinHasBinders DecidabilityBuiltin where
  getBuiltinBinder = \case
    StandardBuiltinFunction Foreach -> Just ForeachBinder
    StandardBuiltinFunction (QuantifyRatTensor q) -> Just $ QuantifierBinder q
    _ -> Nothing

instance BuiltinHasIterate DecidabilityBuiltin where
  accessIterateBuiltin = functionAccessor Iterate

--------------------------------------------------------------------------------
-- Pretty printing

instance Pretty DecidabilityBuiltinTypeClass where
  pretty t = case t of
    HasCompareNat op -> "HasNat" <> pretty op
    HasCompareIndex op -> "HasIndex" <> pretty op
    HasCompareRatTensorPointwise op -> "HasRatTensorPointwise" <> pretty op
    HasBoolTensorLiterals -> pretty $ show t
    IsBoolType -> pretty $ show t
    IsTensorType -> pretty $ show t
    HasNot -> pretty $ show t
    HasAnd -> pretty $ show t
    HasOr -> pretty $ show t
    HasImplies -> pretty $ show t
    HasReduceAndTensor -> pretty $ show t
    HasReduceOrTensor -> pretty $ show t
    HasQuantifyIndex q -> "HasQuantifyIndex" <> braces (pretty q)
    HasQuantifyInList q -> "HasQuantifyInList" <> braces (pretty q)
    HasCompareRatTensorReduced op -> "HasCompareRatTensorReduced" <> pretty op

instance Pretty DecidabilityBuiltinFunction where
  pretty = \case
    TypeTrue -> "true" <> symbol
    TypeFalse -> "false" <> symbol
    TypeNot -> pretty Not <> symbol
    TypeAnd -> pretty And <> symbol
    TypeOr -> pretty Or <> symbol
    TypeImplies -> pretty Implies <> symbol
    TypeCompareNat op -> pretty (CompareNat op) <> symbol
    TypeCompareIndex op -> pretty (CompareIndex op) <> symbol
    TypeCompareRatTensorPointwise op -> pretty (CompareRatTensorPointwise op) <> symbol
    BoolTensorToType -> "boolTensorToType"
    TypeQuantifyIndex q -> pretty (QuantifyIndex q) <> symbol
    TypeQuantifyInList q -> pretty (QuantifyInList q) <> symbol
    where
      symbol = "ᵗ"

instance Pretty DecidabilityBuiltinConstructor where
  pretty = \case
    DecBoolTensor t -> pretty t

instance Pretty DecidabilityBuiltinTypeClassOp where
  pretty t = case t of
    BoolTypeTC -> pretty $ show t
    TensorTypeTC -> pretty $ show t
    FromBoolTensorLitTC -> pretty $ show t
    NotTC -> pretty $ show t
    AndTC -> pretty $ show t
    OrTC -> pretty $ show t
    ImpliesTC -> pretty $ show t
    ReduceAndTensorTC -> pretty $ show t
    ReduceOrTensorTC -> pretty $ show t
    CompareNatTC op -> "CompareNatTC" <> brackets (pretty op)
    CompareIndexTC op -> "CompareIndexTC" <> brackets (pretty op)
    CompareRatTensorPointwiseTC op -> "CompareRatTensorPointwiseTC" <> brackets (pretty op)
    QuantifyIndexTC q -> pretty q <> "IndexTC"
    QuantifyInListTC q -> pretty q <> "InListTC"
    CompareRatTensorReducedTC op -> "CompareRatTensorReducedTC" <> brackets (pretty op)

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
  coercionArgs _ = Nothing

--------------------------------------------------------------------------------
-- Normalisation

instance NormalisableBuiltin DecidabilityBuiltin where
  evalScheme = \case
    StandardBuiltinFunction Iterate -> NonSimple evalIterate
    StandardBuiltinFunction FoldList -> NonSimple evalFoldList
    _ -> None

  blockingArgs = \case
    StandardBuiltinFunction Iterate -> Known [2]
    _ -> Known []

  isTypeClassOp = \case
    DecidabilityBuiltinTypeClassOp {} -> True
    _ -> False

  isCast p e = case e of
    DecidabilityBuiltinFunction BoolTensorToType -> Just $ forceEvalSimpleBuiltin p e evalBoolTensorToType
    _ -> Nothing

evalBoolTensorToType ::
  (MonadNormBuiltin m, HasBuiltinConstructor expr) =>
  TensorOp1Args (expr DecidabilityBuiltin) ->
  m (expr DecidabilityBuiltin)
evalBoolTensorToType args = return $ case args of
  TensorOp1Args _ (getExpr accessBuiltinC -> Just (StandardBuiltinConstructor (BoolTensorLiteral t), [])) -> do
    let op = if anyTensor not t then TypeFalse else TypeTrue
    mkExpr accessBuiltinC (DecidabilityBuiltinFunction op, [])
  _ -> developerError $ "Should not be possible to have non-literal" <+> pretty BoolTensorToType <+> "args"

--------------------------------------------------------------------------------
-- DSL

isTensorType :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
isTensorType tElem = builtin (DecidabilityBuiltinTypeClass IsTensorType) @@ [tElem]

type0IgnoreDims :: DSLExpr DecidabilityBuiltin
type0IgnoreDims = lam "ds" Explicit Irrelevant tDims $ const type0

builtinDerivedFunction :: DerivedFunction -> DSLExpr DecidabilityBuiltin
builtinDerivedFunction = builtin . StandardBuiltinDerivedFunction
