{-# LANGUAGE OverloadedLists #-}

module Vehicle.Backend.ITP.Core where

import Control.Monad.Except (MonadError(..), Except, runExcept)
import Control.Monad.Reader (MonadReader, MonadReader(..), ReaderT(..))
import Data.Text (Text)
import Data.Map (Map)
import Data.Version (Version)
import Data.Void (Void)

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print

-- * Utilities when compiling to an interactive theorem prover backend

--------------------------------------------------------------------------------
-- Backends

data Backend
  = Agda

instance Pretty Backend where
  pretty Agda = "Agda"

--------------------------------------------------------------------------------
-- Options

-- |The options that are specificable by the user when compiling to an ITP
-- backend
data ITPOptions backendOpts = ITPOptions
  { vehicleUIDs  :: Map Text Text
  , aisecVersion :: Version
  , backendOpts  :: backendOpts
  }

-- |Generate the file header given the token used to start comments in the
-- target language
fileHeader :: ITPOptions backendOpts -> Doc a -> Doc a
fileHeader options commentToken = vsep $
  map (commentToken <+>)
    [ "This file was generated automatically by Vehicle"
    , "and should not be modified manually!"
    , "Metadata"
    , " - AISEC version:" <+> pretty (aisecVersion options)
    , " - Date generated: ???"
    ]

--------------------------------------------------------------------------------
-- Control

-- |Constraint for the monad stack used by the Compiler.
type MonadCompile options m =
  (MonadError CompileError m, MonadReader (ITPOptions options) m)

type Compile a options = ReaderT (ITPOptions options) (Except CompileError) a

-- * Type of errors that can be thrown during compilation
data CompileError
  = CompilationUnsupported  Provenance (Doc Void)
  | ContainerDimensionError Provenance ContainerDimensionError
  | NoDecisionProcedureAvailable Provenance Quantifier

instance MeaningfulError CompileError where
  details (CompilationUnsupported p operation) = UError $ UserError
    { provenance = p
    , problem    = "compilation of" <+> squotes operation <+> "is not supported"
    , fix        = "see user manual for details"
    }

  details (ContainerDimensionError p err) = UError $ UserError
    { provenance = p
    , problem    = problem
    , fix        = "see user manual for details"
    }
      where
        problem = case err of

          -- VariableTensorType expr ->
          --   "type of the tensor" <+> squotes (prettyFriendly expr) <+> "is not of the form `Tensor A [...]`"

          -- UnableToInferListSize -> _
          -- VariableTensorTypeDimensions -> _
          -- VariableTensorTypeDimension OutputExpr
          -- EmptyTensorSize

          --ContainerIndexOutOfBounds index size tCont ->
          --  "index" <+> pretty index <+> "is larger than the first dimension" <+> squoutes index <+>
          --  "of the type" <+> prettyFriendly tCont



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
containerType (BuiltinContainerType _ t) = t
containerType t = unexpectedTypeError t (map show [List, Tensor])

data ContainerDimensionError
  = UnableToInferListSize
  | VariableTensorType OutputExpr
  | VariableTensorTypeDimensions OutputExpr
  | VariableTensorTypeDimension OutputExpr
  | EmptyTensorSize
  | ContainerIndexOutOfBounds ContainerType Int Int

containerSize :: OutputExpr -> OutputExpr -> Either ContainerDimensionError Int
containerSize container contType = runExcept result
  where
    result :: Except ContainerDimensionError Int
    result = case contType of
      (App _ (BuiltinContainerType _ List)   _)          -> getListSize container
      (App _ (BuiltinContainerType _ Tensor) [_, tDims]) -> getTensorSize (argExpr tDims)
      _                                                  -> throwError $ VariableTensorType contType

    getListSize :: OutputExpr -> Except ContainerDimensionError Int
    getListSize (App _ (Seq _ xs) _args)              = return $ length xs
    getListSize (App _ (Builtin _ Cons) [_t, _x, xs]) = fmap (1 +) (getListSize (argExpr xs))
    getListSize _                                     = throwError UnableToInferListSize

    getTensorSize :: OutputExpr -> Except ContainerDimensionError Int
    getTensorSize (Seq _ [])       = throwError EmptyTensorSize
    getTensorSize (Seq _ (x : _))  = getDimension x
    getTensorSize t                = throwError $ VariableTensorTypeDimensions t

    getDimension :: OutputExpr -> Except ContainerDimensionError Int
    getDimension (LitNat _ i) = return i
    getDimension t            = throwError $ VariableTensorTypeDimension t