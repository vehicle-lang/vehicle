module Vehicle.Compile.Backend.ITP where

import Control.Monad.Except (MonadError(..), Except, runExcept)

import Vehicle.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.AST
import Vehicle.Language.Print

-- * Utilities when compiling to an interactive theorem prover backend

--------------------------------------------------------------------------------
-- Options

--------------------------------------------------------------------------------
-- Control

-- |Constraint for the monad stack used by the Compiler.
type MonadCompile e m =
  ( MonadLogger m
  , MonadError e m
  )

unexpectedTypeError :: OutputExpr -> [String] -> a
unexpectedTypeError actualType expectedTypes = developerError $
  "Unexpected type found." <+>
  "Was expecting one of" <+> pretty expectedTypes <+>
  "but found" <+> prettyFriendly actualType <+>
  "at" <+> pretty (provenanceOf actualType) <> "."

unexpectedExprError :: Provenance -> OutputExpr -> [String] -> a
unexpectedExprError p actualExpr expectedExprs = developerError $
  "Was expecting something of the form" <+> pretty expectedExprs <+>
  "but found" <+> prettyFriendly actualExpr <+>
  "at" <+> pretty p <> "."

unexpectedArgsError :: OutputExpr -> [OutputExpr] -> [String] -> a
unexpectedArgsError fun actualArgs expectedArgs = developerError $
  "The function" <+> prettyFriendly fun <+> "was expected to have arguments" <+>
  "of the following form" <+> squotes (pretty expectedArgs) <+> "but found" <+>
  "the following" <+> squotes (prettyFriendly actualArgs) <+>
  "at" <+> pretty (provenanceOf fun) <> "."

--------------------------------------------------------------------------------
-- Generic language features

type Precedence = Int

--------------------------------------------------------------------------------
-- Subcategories of types/expressions

numericType :: OutputExpr -> NumericType
numericType (BuiltinNumericType _ t) = t
numericType t = unexpectedTypeError t (map show [Nat, Int, Rat, Real])

booleanType :: OutputExpr -> BooleanType
booleanType (BuiltinBooleanType _ t) = t
booleanType t = unexpectedTypeError t (map show [Bool, Prop])

containerType :: OutputExpr -> ContainerType
containerType (App _ (BuiltinContainerType _ t) _) = t
containerType t = unexpectedTypeError t (map show [List, Tensor])

tensorSize :: OutputExpr -> Either ContainerDimensionError Int
tensorSize tDims = runExcept (getTensorSize (exprHead tDims))
  where
    getTensorSize :: OutputExpr -> Except ContainerDimensionError Int
    getTensorSize (LSeq _ _ [])       = throwError EmptyTensorSize
    getTensorSize (LSeq _ _ (x : _))  = getDimension (exprHead x)
    getTensorSize t                   = throwError $ VariableTensorTypeDimensions t

    getDimension :: OutputExpr -> Except ContainerDimensionError Int
    getDimension (LitNat _ i) = return i
    getDimension t            = throwError $ VariableTensorTypeDimension t