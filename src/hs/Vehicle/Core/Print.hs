{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Core.Print where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Prettyprinter (Pretty(..), Doc, braces, parens, (<+>), line)

import Vehicle.Core.AST
import Vehicle.Prelude

printTree :: Pretty a => a -> Text
printTree = layoutAsText . pretty

brackets :: Visibility -> (Doc a -> Doc a)
brackets Implicit = braces
brackets Explicit = parens

instance Pretty Name where
  pretty Machine       = "machine"
  pretty (User symbol) = pretty symbol

instance Pretty Var where
  pretty (Bound index) = pretty index
  pretty (Free  ident) = pretty ident

instance Pretty Literal where
  pretty = \case
    LNat  x -> pretty x
    LInt  x -> pretty x
    LReal x -> pretty x
    LBool x -> pretty x

instance Pretty Builtin where
  pretty b = pretty $ fromMaybe "" (symbolFromBuiltin b)

instance Pretty (WithProvenance Identifier) where
  pretty (WithProvenance _ann n) = pretty n

instance ( Pretty binder
         , Pretty var
         ) => Pretty (Arg binder var name) where
  pretty (Arg _p vis expr) = visBrackets vis $ pretty expr

instance ( Pretty binder
         , Pretty var
         ) => Pretty (Binder binder var ann) where
  pretty (Binder _ann vis name typ) = brackets vis (pretty name <+> pretty typ)

instance ( Pretty binder
         , Pretty var
         ) => Pretty (Expr binder var ann) where
  pretty = \case
    Type l                      -> "Type" <+> pretty l
    Constraint                  -> "Constraint"
    Hole    _p   name           -> "?" <> pretty name
    Meta    _p   m              -> "?" <> pretty m
    Ann     _ann term typ       -> pretty term <+> ":type" <+> pretty typ
    App     _ann fun arg        -> pretty fun <+> parens (pretty arg)
    Pi      _ann binder res     -> "pi" <+> pretty binder <+> parens (pretty res)
    Builtin _ann op             -> pretty op
    Let     _ann binder e1 e2   -> "let"    <+> pretty binder <+> parens (pretty e1) <+> parens (pretty e2)
    Lam     _ann binder e       -> "lambda" <+> pretty binder <+> parens (pretty e)
    Literal _ann l              -> pretty l
    Seq     _ann es             -> hsep (fmap pretty es)
    Var     _ann v              -> pretty v

instance ( Pretty name
         , Pretty binder
         ) => Pretty (Decl name binder ann) where
  pretty = \case
    DeclNetw _ann n t    -> parens ("declare-network" <+> pretty n <+> ":" <+> pretty t) <+> line
    DeclData _ann n t    -> parens ("declare-dataset" <+> pretty n <+> ":" <+> pretty t) <+> line
    DefFun   _ann n t e  -> parens ("define-fun" <+> pretty n <+> parens (pretty t) <+> parens (pretty e)) <+> line

instance ( Pretty name
         , Pretty binder
         ) => Pretty (Prog name binder ann) where
  pretty (Main ds) = vsep (map pretty ds)