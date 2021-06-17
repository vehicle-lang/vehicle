{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Vehicle.Backend.Core where

import Data.Text as Text (Text, intercalate)
import Vehicle.Core.AST (Tree (..), Expr, Type, Decl, Prog, EArg, TArg)
import Vehicle.Core.Abs (Name(..))
import Vehicle.Prelude (K(..))

-- |Some useful type synonyms
type TransExpr ann = Expr (K Name) ann
type TransType ann = Type (K Name) ann
type TransDecl ann = Decl (K Name) ann
type TransProg ann = Prog (K Name) ann
type TransEArg ann = EArg (K Name) ann
type TransTArg ann = TArg (K Name) ann

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

pattern TOp0 op ann0 = TCon ann0 op
pattern TOp1 op e1 ann0 ann1 = TApp ann1 (TOp0 op ann0 ) e1
pattern TOp2 op e1 e2 ann0 ann1 ann2 = TApp ann2 (TOp1 op e1 ann0 ann1 ) e2

pattern EOp0 op ann0  = ECon ann0 op
pattern EOp1 op e1 ann0 ann1  = EApp ann1 (EOp0 op ann0 ) e1
pattern EOp2 op e1 e2 ann0 ann1 ann2  = EApp ann2 (EOp1 op e1 ann0 ann1 ) e2
pattern EOp3 op e1 e2 e3 ann0 ann1 ann2 ann3   = EApp ann3 (EOp2 op e1 e2 ann0 ann1 ann2 ) e3
