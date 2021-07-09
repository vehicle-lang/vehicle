{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Vehicle.Backend.ITP.Core where

import Control.Monad.Except (MonadError(..), Except)
import Control.Monad.Reader (MonadReader, MonadReader(..), ReaderT(..))
import Data.Text as Text (Text, intercalate, pack, append)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)
import Data.Version (Version, showVersion)
import Prettyprinter as Pretty hiding (squotes)

import Vehicle.Frontend.AST (Tree(..), OutputAnn, OutputType, OutputExpr, annotation)
import Vehicle.Frontend.AST.Info ( Info(..) )
import Vehicle.Frontend.Print ()
import Vehicle.Prelude
import Vehicle.Error

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
fileHeader :: ITPOptions backendOpts -> Text -> Text
fileHeader options commentToken = intercalate "\n" $
  map (append (commentToken <> " "))
    [ "This file was generated automatically by the AISEC tool"
    , "and should not be modified manually!"
    , "Metadata"
    , " - AISEC version: " <> pack (showVersion (aisecVersion options))
    , " - Date generated: ???"
    ]

--------------------------------------------------------------------------------
-- AST abbreviations

annotatedType :: OutputAnn 'EXPR -> OutputType
annotatedType (t :*: _) = unInfo t

--------------------------------------------------------------------------------
-- Control

-- |Constraint for the monad stack used by the Compiler.
type MonadCompile m options =
  (MonadError CompileError m, MonadReader (ITPOptions options) m)

type Compile a options = ReaderT (ITPOptions options) (Except CompileError) a

-- * Type of errors that can be thrown during compilation
data CompileError
  = CompilationUnsupported Provenance Symbol
  | TensorIndexOutOfBounds Provenance OutputType Integer
  | UnexpectedType         Provenance OutputExpr OutputType [Symbol]
  | UnexpectedExpr         Provenance OutputExpr

instance MeaningfulError CompileError where
  details (CompilationUnsupported p symbol) = UError $ UserError
    { provenance = p
    , problem    = "compilation of" <+> squotes symbol <+> "is not supported"
    , fix        = "see user manual for details"
    }

  details (TensorIndexOutOfBounds p tensorTyp index) = UError $ UserError
    { provenance = p
    , problem    = "index" <+> pretty index <+> "is larger than the first dimension of the type" <+> pretty tensorTyp
    , fix        = "check your indexing"
    }

  details (UnexpectedType p expr actualType expectedTypes) = DError $ DeveloperError
    { provenance = p
    , problem    = "unexpected type found for expression" <+> pretty expr <> "." <> line
                   <> "Was expecting one of" <+> list (map pretty expectedTypes) <+> "but found" <+> pretty actualType
    }

  details (UnexpectedExpr p expr) = DError $ DeveloperError
    { provenance = p
    , problem    = "unexpected expression" <+> pretty expr
    }

--------------------------------------------------------------------------------
-- Subcategories of types/expressions

-- * Types of numeric data supported
data NumericType
  = Int
  | Real

numericType :: MonadCompile m options => OutputExpr -> m NumericType
numericType expr = go $ annotatedType (annotation expr)
  where
    go :: MonadCompile m options => OutputType -> m NumericType
    go = \case
      TInt  _ann        -> return Int
      TReal _ann        -> return Real
      TFun  _ann _t1 t2 -> go t2
      typ               -> throwError $ UnexpectedType (prov expr) expr typ ["Real", "Int", "X -> Bool", "X -> Prop"]

-- |Types of boolean data supported
data BoolType
  = Bool
  | Prop

booleanType :: MonadCompile m options => OutputExpr -> m BoolType
booleanType expr = go (annotatedType (annotation expr))
  where
    go :: MonadCompile m options => OutputType -> m BoolType
    go = \case
      TBool _ann        -> return Bool
      TProp _ann        -> return Prop
      TFun  _ann _t1 t2 -> go t2
      typ               -> throwError $ UnexpectedType (prov expr) expr typ ["Bool", "Prop", "X -> Bool", "X -> Prop"]

-- | Types of container data supported
data ContainerType
  = List
  | Tensor (NonEmpty Integer)

containerType :: MonadCompile m options => OutputExpr -> m ContainerType
containerType expr = let ann = annotation expr in case annotatedType ann of
  TList   _ _                    -> return List
  TTensor _ _ (TLitDimList _ ds) -> Tensor <$> traverse fromLit ds
    where
      fromLit :: MonadCompile m options => OutputType -> m Integer
      fromLit (TLitDim _ i) = return i
      fromLit t             = throwError $ UnexpectedType (prov ann) expr t ["a literal"]
  t              -> throwError $ UnexpectedType (prov ann) expr t ["List", "Tensor"]

-- |Types of numeric orders
data OrderType
  = Leq
  | Lt
  | Geq
  | Gt

-- |Types of numeric unary operations
data NumericOp1
  = Neg

-- |Types of numeric binary operations
data NumericOp2
  = Mul
  | Div
  | Add
  | Sub

-- |Types of boolean unary operations
data BooleanOp1
  = Not

-- |Types of boolean binary operations
data BooleanOp2
  = Impl
  | And
  | Or

type Precedence = Int
