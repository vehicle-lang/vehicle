module Vehicle.Syntax.Print
  ( Printable (printInternal, printExternal),
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter (Doc, Pretty (..))
import Vehicle.Syntax.AST
import Vehicle.Syntax.BNFC.Delaborate.External as External (delab)
import Vehicle.Syntax.BNFC.Delaborate.Internal as Internal (delab)
import Vehicle.Syntax.External.Print as External (printTree)
import Vehicle.Syntax.Internal.Print as Internal (printTree)

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

instance Printable Arg where
  printInternal' = Internal.printTree . Internal.delab
  printExternal' = External.printTree . External.delab

instance Printable Binder where
  printInternal' = Internal.printTree . Internal.delab
  printExternal' = External.printTree . External.delab

instance Printable Expr where
  printInternal' = Internal.printTree . Internal.delab
  printExternal' = External.printTree . External.delab

instance Printable Decl where
  printInternal' = Internal.printTree . Internal.delab
  printExternal' = External.printTree . External.delab

instance Printable Prog where
  printInternal' = Internal.printTree . Internal.delab
  printExternal' = External.printTree . External.delab

-- BNFC printer treats the braces for implicit arguments as layout braces and
-- therefore adds a ton of tree structured new-lines everywhere. This hack attempts to undo this.
bnfcPrintHack :: String -> Text
bnfcPrintHack = go removeTrailingSpace . removeNewLines . go leftAlignBrackets . Text.pack
  where
    go :: (Text -> Text) -> Text -> Text
    go f t = do
      let t' = f t
      if t == t'
        then t'
        else go f t'

    leftAlignBrackets :: Text -> Text
    leftAlignBrackets =
      Text.replace "  {" "{"
        . Text.replace "  }" "}"

    removeNewLines :: Text -> Text
    removeNewLines =
      Text.replace "\n{" " {"
        . Text.replace "{\n" "{"
        . Text.replace "\n}" "}"
        . Text.replace "}\n" "} "

    removeTrailingSpace :: Text -> Text
    removeTrailingSpace =
      Text.replace "{  " "{"
        . Text.replace "}  " "}"
