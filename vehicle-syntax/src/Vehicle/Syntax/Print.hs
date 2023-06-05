module Vehicle.Syntax.Print
  ( Printable (printInternal, printExternal),
  )
where

import Control.Monad.Identity (Identity (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter (Doc, Pretty (..))
import Vehicle.Syntax.AST
import Vehicle.Syntax.BNFC.Delaborate.External as External (Delaborate, delab)
import Vehicle.Syntax.BNFC.Delaborate.Internal as Internal (delab)
import Vehicle.Syntax.External.Abs qualified as BF
import Vehicle.Syntax.External.Print as External (Print, printTree)
import Vehicle.Syntax.Internal.Abs qualified as BC
import Vehicle.Syntax.Internal.Print as Internal (Print, printTree)

--------------------------------------------------------------------------------
-- Conversion to BNFC representation

class Printable a where
  printInternal' :: a -> String
  printExternal' :: a -> String

  -- | Prints to a Lisp-like language for debugging
  printInternal :: a -> Doc b
  printInternal = pretty . bnfcPrintHack . printInternal'

  -- | Prints to the user surface syntax.
  printExternal :: a -> Doc b
  printExternal = pretty . bnfcPrintHack . printExternal'

instance Printable (Arg Name Builtin) where
  printInternal' = Internal.printTree . Internal.delab
  printExternal' = External.printTree . External.delab

instance Printable (Binder Name Builtin) where
  printInternal' = Internal.printTree . Internal.delab
  printExternal' = External.printTree . External.delab

instance Printable (Expr Name Builtin) where
  printInternal' = Internal.printTree . Internal.delab
  printExternal' = External.printTree . External.delab

instance Printable (Decl Name Builtin) where
  printInternal' = Internal.printTree . Internal.delab
  printExternal' = External.printTree . External.delab

instance Printable (Prog Name Builtin) where
  printInternal' = Internal.printTree . Internal.delab
  printExternal' = External.printTree . External.delab

-- BNFC printer treats the braces for implicit arguments as layout braces and
-- therefore adds a ton of newlines everywhere. This hack attempts to undo this.
bnfcPrintHack :: String -> Text
bnfcPrintHack =
  Text.replace "{{ " "{{"
    . Text.replace "{  " "{"
    . Text.replace "\n{" " {"
    . Text.replace "{\n" "{"
    . Text.replace "\n}" "}"
    . Text.replace "}\n" "} "
    . Text.pack
