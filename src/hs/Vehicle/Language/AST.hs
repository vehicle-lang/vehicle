module Vehicle.Language.AST
  ( module X
  , pattern InferableOption
  ) where

import Vehicle.Language.AST.Builtin as X
import Vehicle.Language.AST.CoDeBruijn as X
import Vehicle.Language.AST.Core as X
import Vehicle.Language.AST.DeBruijn as X
import Vehicle.Language.AST.Name as X
import Vehicle.Language.AST.Position as X
import Vehicle.Language.AST.Provenance as X
import Vehicle.Language.AST.Relevance as X
import Vehicle.Language.AST.Visibility as X

import Data.Text

--------------------------------------------------------------------------------
-- Resource options

pattern InferableOption :: Text
pattern InferableOption = "infer"
