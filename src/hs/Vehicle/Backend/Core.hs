{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Vehicle.Backend.Core where

import Data.Text as Text (Text, intercalate)
import Vehicle.Core.AST (Tree (..), Expr, Type, Decl, Prog, EArg, TArg, Builtin(..))
import Vehicle.Core.Abs (Name(..))
import Vehicle.Prelude (K(..))

-- |Some useful type synonyms
type TransExpr ann = Expr (K Name) Builtin ann
type TransType ann = Type (K Name) Builtin ann
type TransDecl ann = Decl (K Name) Builtin ann
type TransProg ann = Prog (K Name) Builtin ann
type TransEArg ann = EArg (K Name) Builtin ann
type TransTArg ann = TArg (K Name) Builtin ann

-- |Generate the file header given the token used to start comments in the target language
fileHeader :: Text -> Text
fileHeader commentToken = intercalate ("\n" <> commentToken <> " ") [
    "This file was generated automatically by the AISEC tool version X",
    "and should not be modified manually!",
    "Metadata",
    " - AISEC version ???",
    " - Date generated ???"
  ]



---------------------
-- Useful patterns --
---------------------

-- Perhaps these are useful elsewhere and should be lifted?

pattern TOp0 op ann0 pos = TCon ann0 (Builtin pos op)
pattern TOp1 op e1 ann0 ann1 pos = TApp ann1 (TOp0 op ann0 pos) e1
pattern TOp2 op e1 e2 ann0 ann1 ann2 pos = TApp ann2 (TOp1 op e1 ann0 ann1 pos) e2

pattern EOp0 op ann0 pos = ECon ann0 (Builtin pos op)
pattern EOp1 op e1 ann0 ann1 pos = EApp ann1 (EOp0 op ann0 pos) e1
pattern EOp2 op e1 e2 ann0 ann1 ann2 pos = EApp ann2 (EOp1 op e1 ann0 ann1 pos) e2
pattern EOp3 op e1 e2 e3 ann0 ann1 ann2 ann3 pos  = EApp ann3 (EOp2 op e1 e2 ann0 ann1 ann2 pos) e3
