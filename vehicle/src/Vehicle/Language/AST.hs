module Vehicle.Language.AST
  ( module X
  , pattern InferableOption
  ) where

import Vehicle.Language.AST.Arg as X
import Vehicle.Language.AST.Binder as X
import Vehicle.Language.AST.Builtin as X
import Vehicle.Language.AST.CoDeBruijn as X
import Vehicle.Language.AST.DeBruijn as X
import Vehicle.Language.AST.Decl as X
import Vehicle.Language.AST.Expr as X
import Vehicle.Language.AST.Meta as X
import Vehicle.Language.AST.Name as X
import Vehicle.Language.AST.Position as X
import Vehicle.Language.AST.Prog as X
import Vehicle.Language.AST.Provenance as X
import Vehicle.Language.AST.Relevance as X
import Vehicle.Language.AST.Visibility as X

import Data.Text

--------------------------------------------------------------------------------
-- Resource options

pattern InferableOption :: Text
pattern InferableOption = "infer"
