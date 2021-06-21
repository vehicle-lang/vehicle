{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DataKinds #-}
module Vehicle.Backend.ITP.Core where

import Data.Text as Text (Text, intercalate, pack)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (map)
import Data.Version (Version)

import Prettyprinter as Pretty

import Control.Monad.Except (MonadError(..), Except)
import Control.Monad.Reader (MonadReader, MonadReader(..), ReaderT(..))


import Prettyprinter.Internal as P

import Vehicle.Frontend.AST (Tree(..), Expr, Type, Decl, Prog, Kind, EArg, TArg)
import Vehicle.Frontend.AST.Info ( Info(..) )
import Vehicle.Prelude (K(..), Sort(..), Provenance, (:*:)(..), Symbol)

-- * Utilities when compiling to an interactive theorem prover backend

--------------------------------------------------------------------------------
-- Options

-- | Options
data ITPOptions o = ITPOptions
  { vehicleUIDs     :: Map Text Text
  , aisecVersion    :: Version
  , filepath        :: Text
  , backendOptions  :: o
  }

-- |Generate the file header given the token used to start comments in the
-- target language
fileHeader :: ITPOptions o -> Text -> Text
fileHeader options commentToken = intercalate ("\n" <> commentToken <> " ")
  [ "This file was generated automatically by the AISEC tool"
  , "and should not be modified manually!"
  , "Metadata"
  , " - AISEC version: " <> pack (show (aisecVersion options))
  , " - Date generated: ???"
  ]

--------------------------------------------------------------------------------
-- AST abbreviations

-- |The type of annotations used
type TAnn = Info :*: K Provenance

annotatedType :: TAnn 'EXPR -> TType
annotatedType (t :*: _) = unInfo t

annotatedProv :: TAnn sort -> Provenance
annotatedProv (_ :*: K p) = p

-- * Some useful type synonyms

type TTree = Tree TAnn
type TKind = Kind TAnn
type TType = Type TAnn
type TExpr = Expr TAnn
type TDecl = Decl TAnn
type TProg = Prog TAnn
type TEArg = EArg TAnn
type TTArg = TArg TAnn

--------------------------------------------------------------------------------
-- Control

-- |Constraint for the monad stack used by the Compiler.
type MonadCompile m options =
  (MonadError CompileError m, MonadReader (ITPOptions options) m)

type Compile a options = ReaderT (ITPOptions options) (Except CompileError) a

-- * Type of errors that can be thrown during compilation
data CompileError
  -- User errors
  = CompilationUnsupported Symbol Provenance
  | IndexOutOfBounds TType Integer Provenance
  -- Vehicle errors
  | UnexpectedType TType Provenance
  | UnexpectedExpr TExpr Provenance

--------------------------------------------------------------------------------
-- Subcategories of types/expressions

-- * Types of numeric data supported
data NumericType
  = Int
  | Real

numericType :: MonadCompile m options => TAnn 'EXPR -> m NumericType
numericType ann = case annotatedType ann of
  TInt  _ -> return Int
  TReal _ -> return Real
  t       -> throwError $ UnexpectedType t (annotatedProv ann)

-- |Types of boolean data supported
data BoolType
  = Bool
  | Prop

booleanType :: MonadCompile m options => TAnn 'EXPR -> m BoolType
booleanType ann = case annotatedType ann of
  TBool _ -> return Bool
  TProp _ -> return Prop
  _       -> throwError $ UnexpectedType (annotatedType ann) (annotatedProv ann)

-- | Types of container data supported
data ContainerType
  = List
  | Tensor (NonEmpty Integer)

containerType :: MonadCompile m options => TAnn 'EXPR -> m ContainerType
containerType ann = case annotatedType ann of
  TList   _ _                    -> return List
  TTensor _ _ (TLitDimList _ ds) -> Tensor <$> traverse fromLit ds
    where
      fromLit :: MonadCompile m options => TType -> m Integer
      fromLit (TLitDim _ i) = return i
      fromLit _             = throwError $ UnexpectedType (annotatedType ann) (annotatedProv ann)
  _              -> throwError $ UnexpectedType (annotatedType ann) (annotatedProv ann)

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

docAnn :: Doc ann -> Maybe ann
docAnn (Annotated a _) = Just a
docAnn _               = Nothing


--------------------------------------------------------------------------------
-- Prettyprinter

-- Redefining some pretty printer primitives to work with `Foldable`.
-- Can remove once https://github.com/quchen/prettyprinter/pull/200 is released.
hsep :: Foldable t => t (Doc ann) -> Doc ann
hsep = concatWith (<+>)

vsep :: Foldable t => t (Doc ann) -> Doc ann
vsep = concatWith (\x y -> x <> line <> y)

hcat :: Foldable t => t (Doc ann) -> Doc ann
hcat = concatWith (<>)

vcat :: Foldable t => t (Doc ann) -> Doc ann
vcat = concatWith (\x y -> x <> line' <> y)