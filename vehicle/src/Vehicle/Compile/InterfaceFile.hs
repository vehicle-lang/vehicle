
module Vehicle.Compile.InterfaceFile where

import Data.Text (Text)

import Vehicle.Compile.Prelude

data InterfaceFile = InterfaceFile
  { fileHash   :: Int
  , serialised :: CheckedExpr
  }

loadInterfaceFile :: Text -> Maybe CheckedProg
loadInterfaceFile interfaceContent = _
