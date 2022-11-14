-- File generated by the BNF Converter (bnfc 2.9.3).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Vehicle.

module Vehicle.Core.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Vehicle.Core.Abs
import qualified Data.Text

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Vehicle.Core.Abs.BuiltinToken where
  prt _ (Vehicle.Core.Abs.BuiltinToken (_,i)) = doc $ showString (Data.Text.unpack i)
instance Print Vehicle.Core.Abs.NameToken where
  prt _ (Vehicle.Core.Abs.NameToken (_,i)) = doc $ showString (Data.Text.unpack i)
instance Print Vehicle.Core.Abs.BoolToken where
  prt _ (Vehicle.Core.Abs.BoolToken (_,i)) = doc $ showString (Data.Text.unpack i)
instance Print Vehicle.Core.Abs.HoleToken where
  prt _ (Vehicle.Core.Abs.HoleToken (_,i)) = doc $ showString (Data.Text.unpack i)
instance Print Vehicle.Core.Abs.TypeToken where
  prt _ (Vehicle.Core.Abs.TypeToken (_,i)) = doc $ showString (Data.Text.unpack i)
instance Print Vehicle.Core.Abs.Rational where
  prt _ (Vehicle.Core.Abs.Rational (_,i)) = doc $ showString (Data.Text.unpack i)
instance Print Vehicle.Core.Abs.Binder where
  prt i = \case
    Vehicle.Core.Abs.ExplicitBinder nametoken expr -> prPrec i 0 (concatD [doc (showString "("), prt 0 nametoken, doc (showString ":type"), prt 0 expr, doc (showString ")")])
    Vehicle.Core.Abs.ImplicitBinder nametoken expr -> prPrec i 0 (concatD [doc (showString "{"), prt 0 nametoken, doc (showString ":type"), prt 0 expr, doc (showString "}")])
    Vehicle.Core.Abs.InstanceBinder nametoken expr -> prPrec i 0 (concatD [doc (showString "{{"), prt 0 nametoken, doc (showString ":type"), prt 0 expr, doc (showString "}}")])

instance Print Vehicle.Core.Abs.Arg where
  prt i = \case
    Vehicle.Core.Abs.ExplicitArg expr -> prPrec i 0 (concatD [prt 1 expr])
    Vehicle.Core.Abs.ImplicitArg expr -> prPrec i 0 (concatD [doc (showString "{"), prt 0 expr, doc (showString "}")])
    Vehicle.Core.Abs.InstanceArg expr -> prPrec i 0 (concatD [doc (showString "{{"), prt 0 expr, doc (showString "}}")])

instance Print Vehicle.Core.Abs.Lit where
  prt i = \case
    Vehicle.Core.Abs.LitInt n -> prPrec i 0 (concatD [prt 0 n])
    Vehicle.Core.Abs.LitRat rational -> prPrec i 0 (concatD [prt 0 rational])
    Vehicle.Core.Abs.LitBool booltoken -> prPrec i 0 (concatD [prt 0 booltoken])

instance Print Vehicle.Core.Abs.Expr where
  prt i = \case
    Vehicle.Core.Abs.Ann expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString ":type"), prt 1 expr2])
    Vehicle.Core.Abs.Pi binder expr -> prPrec i 0 (concatD [doc (showString "pi"), prt 0 binder, prt 1 expr])
    Vehicle.Core.Abs.Let binder expr1 expr2 -> prPrec i 0 (concatD [doc (showString "let"), prt 0 binder, prt 1 expr1, prt 1 expr2])
    Vehicle.Core.Abs.Lam binder expr -> prPrec i 0 (concatD [doc (showString "lambda"), prt 0 binder, prt 1 expr])
    Vehicle.Core.Abs.App expr arg -> prPrec i 0 (concatD [prt 1 expr, prt 0 arg])
    Vehicle.Core.Abs.Type typetoken -> prPrec i 1 (concatD [prt 0 typetoken])
    Vehicle.Core.Abs.Builtin builtintoken -> prPrec i 1 (concatD [prt 0 builtintoken])
    Vehicle.Core.Abs.Var nametoken -> prPrec i 1 (concatD [prt 0 nametoken])
    Vehicle.Core.Abs.Literal lit -> prPrec i 1 (concatD [prt 0 lit])
    Vehicle.Core.Abs.LSeq exprs -> prPrec i 1 (concatD [doc (showString "["), prt 1 exprs, doc (showString "]")])
    Vehicle.Core.Abs.Hole holetoken -> prPrec i 1 (concatD [prt 0 holetoken])

instance Print [Vehicle.Core.Abs.Expr] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 1 x, prt 1 xs]

instance Print Vehicle.Core.Abs.Decl where
  prt i = \case
    Vehicle.Core.Abs.DeclNetw nametoken expr -> prPrec i 0 (concatD [doc (showString "("), doc (showString "declare-network"), prt 0 nametoken, prt 1 expr, doc (showString ")")])
    Vehicle.Core.Abs.DeclData nametoken expr -> prPrec i 0 (concatD [doc (showString "("), doc (showString "declare-dataset"), prt 0 nametoken, prt 1 expr, doc (showString ")")])
    Vehicle.Core.Abs.DefFun nametoken expr1 expr2 -> prPrec i 0 (concatD [doc (showString "("), doc (showString "define-fun"), prt 0 nametoken, prt 1 expr1, prt 1 expr2, doc (showString ")")])

instance Print [Vehicle.Core.Abs.Decl] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Vehicle.Core.Abs.Prog where
  prt i = \case
    Vehicle.Core.Abs.Main decls -> prPrec i 0 (concatD [doc (showString "("), prt 0 decls, doc (showString ")")])
