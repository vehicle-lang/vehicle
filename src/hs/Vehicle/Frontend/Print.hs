{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Frontend.Print where

import Data.Text (Text)
import Prettyprinter (Pretty(pretty), unAnnotate, layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.Text (renderStrict)
import Vehicle.Prelude
import Vehicle.Frontend.AST.Core (Tree)
import Vehicle.Backend.ITP.Vehicle (compile)

instance KnownSort sort => Pretty (Tree ann sort) where
  pretty t = unAnnotate (compile t)

print :: KnownSort sort => Tree ann sort -> Text
print t = renderStrict $ layoutPretty defaultLayoutOptions (pretty t)