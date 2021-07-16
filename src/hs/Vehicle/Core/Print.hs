{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Core.Print where

import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Prettyprinter (Pretty(..), Doc, layoutPretty, braces, parens, (<+>), line, defaultLayoutOptions)
import Prettyprinter.Render.Text (renderStrict)

import Vehicle.Core.AST
import Vehicle.Prelude (hsep, vsep, Visibility(..))

printTree :: Pretty a => a -> Text
printTree a = renderStrict $ layoutPretty defaultLayoutOptions $ pretty a

brackets :: Visibility -> (Doc a -> Doc a)
brackets Implicit = braces
brackets Explicit = parens

instance Pretty Name where
  pretty Machine       = "machine"
  pretty (User symbol) = pretty symbol

instance Pretty Index where
  pretty (Index index) = pretty index

instance Pretty Literal where
  pretty = \case
    LitNat  x -> pretty x
    LitInt  x -> pretty x
    LitReal x -> pretty x
    LitBool x -> pretty x

instance Pretty Builtin where
  pretty b = pretty $ fromMaybe "" (symbolFromBuiltin b)

instance Pretty (Ident ann) where
  pretty (Ident _ann n) = pretty n

instance ( Pretty binder
         , Pretty var
         ) => Pretty (Arg binder var name) where
  pretty (Arg Explicit expr) = pretty expr
  pretty (Arg Implicit expr) = "{" <> pretty expr <> "}"

instance ( Pretty binder
         , Pretty var
         ) => Pretty (PiBinder binder var ann) where
  pretty (PiBinder _ann vis name typ) = brackets vis (pretty name <+> pretty typ)

instance ( Pretty binder
         , Pretty var
         ) => Pretty (LamBinder binder var ann) where
  pretty (LamBinder _ann vis name typ) = brackets vis (pretty name <+> pretty typ)

instance ( Pretty binder
         , Pretty var
         ) => Pretty (Expr binder var ann) where
  pretty = \case
    Kind                        -> "kind"
    App     _ann fun arg        -> pretty fun <+> parens (pretty arg)
    Pi      _ann binder res     -> "pi" <+> pretty binder <+> parens (pretty res)
    Builtin _ann op             -> pretty op
    Meta    _ann m              -> "?" <> pretty m
    Let     _ann binder e1 e2   -> "let"     <+> pretty binder <+> parens (pretty e1) <+> parens (pretty e2)
    Lam     _ann binder e       -> "lambda"  <+> pretty binder <+> parens (pretty e)
    Literal _ann l              -> pretty l
    Seq     _ann es             -> hsep (fmap pretty es)
    Free    _ann ident          -> pretty ident
    Bound   _ann i              -> parens (pretty i)

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
  pretty (Main ds) = vsep (map pretty ds)