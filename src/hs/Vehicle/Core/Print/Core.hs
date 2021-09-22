{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Core.Print.Core
  ( prettyCore
  , showCore
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.IntSet qualified as IntSet ( toList )
import Data.IntMap qualified as IntMap ( toAscList )
import Prettyprinter (Pretty(..), Doc, braces, concatWith, softline, group, align, parens, (<+>), line)

import Vehicle.Core.AST
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Printing to Core language

prettyCore :: Pretty a => a -> Text
prettyCore = layoutAsText . pretty

showCore :: Pretty a => a -> String
showCore = layoutAsString . pretty

brackets :: Visibility -> (Doc a -> Doc a)
brackets Explicit   = parens
brackets Implicit   = braces
brackets Constraint = braces . braces

instance Pretty Name where
  pretty Machine       = "_"
  pretty (User symbol) = pretty symbol

instance Pretty Var where
  pretty (Free  ident) = pretty ident
  pretty (Bound index)
    | index >= 0 = "i" <> pretty index
    | otherwise  = "i[" <> pretty index <> "]"

instance Pretty Literal where
  pretty = \case
    LNat  x -> pretty x
    LInt  x -> pretty x
    LRat  x -> pretty x
    LBool x -> pretty x

instance Pretty Builtin where
  pretty b = pretty $ fromMaybe "" (symbolFromBuiltin b)

instance Pretty (WithProvenance Identifier) where
  pretty (WithProvenance _ann n) = pretty n

instance Pretty var => Pretty (Arg var name) where
  pretty (Arg _p v expr) = visBrackets v $ pretty expr

instance Pretty var => Pretty (Binder var ann) where
  pretty (Binder _ann v n t) = brackets v (pretty n <+> ":type" <+> pretty t)

instance Pretty var => Pretty (Expr var ann) where
  pretty = \case
    Type l                      -> "Type" <> pretty l
    Hole    _p   name           -> "h?" <> pretty name
    Meta    _p   m              -> "?" <> pretty m
    Ann     _ann term typ       -> parens (pretty term <+> ":type" <+> pretty typ)
    App     _ann fun arg        -> parens (pretty fun <+> pretty arg)
    Pi      _ann binder res     -> parens ("pi" <+> pretty binder <+> pretty res)
    Let     _ann e1 binder e2   -> parens ("let" <+> pretty binder <+> pretty e1 <+> pretty e2)
    Lam     _ann binder e       -> parens ("lambda" <+> pretty binder <+> pretty e)
    Builtin _ann op             -> pretty op
    Literal _ann l              -> pretty l
    Seq     _ann es             -> "[" <> hsep (fmap pretty es) <> "]"
    Var     _ann v              -> pretty v
    PrimDict e                  -> "primDict" <+> pretty e

instance Pretty var => Pretty (Decl var ann) where
  pretty = \case
    DeclNetw _ann n t    -> parens ("declare-network" <+> pretty n <+> ":" <+> pretty t) <+> line
    DeclData _ann n t    -> parens ("declare-dataset" <+> pretty n <+> ":" <+> pretty t) <+> line
    DefFun   _ann n t e  -> parens ("define-fun" <+> pretty n <+> pretty t <+> pretty e) <+> line

instance Pretty var => Pretty (Prog var ann) where
  pretty (Main ds) = vsep (fmap pretty ds)