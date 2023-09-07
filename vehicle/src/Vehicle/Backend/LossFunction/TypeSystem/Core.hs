module Vehicle.Backend.LossFunction.TypeSystem.Core where

import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics
import Prettyprinter (Pretty (..))
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.DSL
import Vehicle.Data.NormalisedExpr
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin (BuiltinConstructor, BuiltinFunction, BuiltinType (..))
import Vehicle.Syntax.Builtin qualified as S
import Vehicle.Syntax.Builtin.BasicOperations

--------------------------------------------------------------------------------
-- Loss type classes

data LossTypeClass
  = IsBoolType
  | HasBoolLiteral Bool
  | HasNot
  | HasAnd
  | HasOr
  | HasImplies
  | HasRatOrder OrderOp
  | HasRatEq EqualityOp
  | HasQuant Quantifier
  | ValidPropertyBaseType
  deriving (Show, Eq, Ord, Generic)

instance Hashable LossTypeClass

instance Pretty LossTypeClass where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- Loss type class operations

data LossTypeClassOp
  = LBoolTC Bool
  | NotTC
  | AndTC
  | OrTC
  | ImpliesTC
  | RatOrderTC OrderOp
  | RatEqTC EqualityOp
  | QuantTC Quantifier
  deriving (Show, Eq, Ord, Generic)

instance Hashable LossTypeClassOp

instance Pretty LossTypeClassOp where
  pretty = pretty . show

-----------------------------------------------------------------------------
-- Type synonyms

data LossBuiltin
  = BuiltinConstructor BuiltinConstructor
  | BuiltinFunction BuiltinFunction
  | BuiltinType BuiltinType
  | NatInDomainConstraint
  | LossTC LossTypeClass
  | LossTCOp LossTypeClassOp
  deriving (Show, Eq, Ord, Generic)

instance Hashable LossBuiltin

instance Pretty LossBuiltin where
  pretty = pretty . show

instance HasStandardTypes LossBuiltin where
  mkBuiltinType = BuiltinType
  getBuiltinType = \case
    BuiltinType c -> Just c
    _ -> Nothing

  mkNatInDomainConstraint = NatInDomainConstraint

instance HasStandardData LossBuiltin where
  mkBuiltinFunction = BuiltinFunction
  getBuiltinFunction = \case
    BuiltinFunction c -> Just c
    _ -> Nothing

  mkBuiltinConstructor = BuiltinConstructor
  getBuiltinConstructor = \case
    BuiltinConstructor c -> Just c
    _ -> Nothing

  isTypeClassOp = \case
    LossTCOp {} -> True
    _ -> False

instance PrintableBuiltin LossBuiltin where
  convertBuiltin p = \case
    BuiltinConstructor c -> Builtin p (S.BuiltinConstructor c)
    BuiltinFunction f -> Builtin p (S.BuiltinFunction f)
    BuiltinType f -> Builtin p (S.BuiltinType f)
    b -> cheatConvertBuiltin p b

  isCoercion = const False

-----------------------------------------------------------------------------
-- Type synonyms

-- Constraint
type LossConstraintProgress = ConstraintProgress LossBuiltin

type LossInstanceConstraint = InstanceConstraint LossBuiltin

type LossUnificationConstraint = UnificationConstraint LossBuiltin

type LossConstraintContext = ConstraintContext LossBuiltin

type LossConstraint = Constraint LossBuiltin

-- WHNFValue
type LossNormExpr = WHNFValue LossBuiltin

type LossNormBinder = WHNFBinder LossBuiltin

type LossNormArg = WHNFArg LossBuiltin

type LossNormType = WHNFType LossBuiltin

type LossSpine = WHNFSpine LossBuiltin

--------------------------------------------------------------------------------
-- DSL

type LossDSLExpr = DSLExpr LossBuiltin

lossTypeClass :: LossTypeClass -> NonEmpty LossDSLExpr -> LossDSLExpr
lossTypeClass tc args = builtin (LossTC tc) @@ args

isBoolType :: LossDSLExpr -> LossDSLExpr
isBoolType t = lossTypeClass IsBoolType [t]

hasBoolLiteral :: Bool -> LossDSLExpr -> LossDSLExpr
hasBoolLiteral b t = lossTypeClass (HasBoolLiteral b) [t]

hasNot :: LossDSLExpr -> LossDSLExpr -> LossDSLExpr
hasNot t1 t2 = lossTypeClass HasNot [t1, t2]

hasAnd :: LossDSLExpr -> LossDSLExpr -> LossDSLExpr -> LossDSLExpr
hasAnd t1 t2 t3 = lossTypeClass HasAnd [t1, t2, t3]

hasOr :: LossDSLExpr -> LossDSLExpr -> LossDSLExpr -> LossDSLExpr
hasOr t1 t2 t3 = lossTypeClass HasOr [t1, t2, t3]

hasImplies :: LossDSLExpr -> LossDSLExpr -> LossDSLExpr -> LossDSLExpr
hasImplies t1 t2 t3 = lossTypeClass HasImplies [t1, t2, t3]

hasRatOrder :: OrderOp -> LossDSLExpr -> LossDSLExpr -> LossDSLExpr -> LossDSLExpr
hasRatOrder ord t1 t2 t3 = lossTypeClass (HasRatOrder ord) [t1, t2, t3]

hasRatEq :: EqualityOp -> LossDSLExpr -> LossDSLExpr -> LossDSLExpr -> LossDSLExpr
hasRatEq eq t1 t2 t3 = lossTypeClass (HasRatEq eq) [t1, t2, t3]

hasQuant :: Quantifier -> LossDSLExpr -> LossDSLExpr -> LossDSLExpr
hasQuant q t1 t2 = lossTypeClass (HasQuant q) [t1, t2]

validPropertyBaseType :: LossDSLExpr -> LossDSLExpr
validPropertyBaseType t = lossTypeClass ValidPropertyBaseType [t]
