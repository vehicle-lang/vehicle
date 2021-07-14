{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Core.Print where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.List.NonEmpty as NonEmpty (toList)

import Prettyprinter (Pretty(..), layoutPretty, parens, (<+>), line, defaultLayoutOptions)
import Prettyprinter.Render.Text (renderStrict)

import Vehicle.Core.AST
import Vehicle.Prelude ( hsep, vsep )

printTree :: Pretty a => a -> Text
printTree a = renderStrict $ layoutPretty defaultLayoutOptions $ pretty a

instance Pretty DeBruijnIndex where
  pretty (Index index) = pretty index

instance Pretty Literal where
  pretty = \case
    LitNat  x -> pretty x
    LitInt  x -> pretty x
    LitReal x -> pretty x
    LitBool x -> pretty x

instance Pretty DeBruijnBinder where
  pretty Machine = "Machine"
  pretty (User symbol) = pretty symbol

instance Pretty (Builtin AbstractBuiltinOp) where
  pretty b = pretty $ fromMaybe "" (symbolFromBuiltin b)

instance Pretty name => Pretty (Binder name ann) where
  pretty (Binder _ann name ) = pretty name

instance ( Pretty name
         , Pretty binder
         ) => Pretty (Expr name binder ann) where
  pretty = \case
    Star    _ann                -> "Star"
    App     _ann e1 e2          -> "App"     <+> pretty e1 <+> parens (pretty e2)
    Fun     _ann e1 e2          -> "Fun"     <+> parens (pretty e1) <+> parens (pretty e2)
    Builtin _ann op             -> "Builtin" <+> pretty op
    Meta    _ann m              -> "Meta"    <+> pretty m
    Forall  _ann binder _cons e -> "Forall"  <+> parens (pretty binder) <+> parens (pretty e)
    Let     _ann binder e1 e2   -> "Let"     <+> parens (pretty binder) <+> pretty e1 <+> pretty e2
    Lam     _ann binder e       -> "Lambda"  <+> parens (pretty binder) <+> parens (pretty e)
    Literal _ann l              -> "Literal" <+> pretty l
    Seq     _ann es             -> "Seq"     <+> hsep (fmap pretty es)
    Free    _ann ident          -> "Free"    <+> pretty ident
    Bound   _ann i              -> "Bound"   <+> parens (pretty i)

instance ( Pretty name
         , Pretty binder
         ) => Pretty (Decl name binder ann) where
  pretty = \case
    DeclNetw _ann n t    -> parens ("declare-network" <+> pretty n <+> ":" <+> pretty t) <+> line
    DeclData _ann n t    -> parens ("declare-dataset" <+> pretty n <+> ":" <+> pretty t) <+> line
    DefType  _ann n ns t -> parens ("define-type" <+> pretty n <+> parens (pretty ns) <+> pretty t) <+> line
    DefFun   _ann n t e  -> parens ("define-fun" <+> pretty n <+> parens (pretty t) <+> parens (pretty e)) <+> line

instance ( Pretty name
         , Pretty binder
         ) => Pretty (Prog name binder ann) where
  pretty (Main ds) = vsep (map pretty (NonEmpty.toList ds))