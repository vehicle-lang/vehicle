module Vehicle.Compile.Prelude
  ( module X,
    module Vehicle.Compile.Prelude,
    Ix (..),
    Lv (..),
  )
where

import Vehicle.Compile.Context.Bound.Core as X
import Vehicle.Compile.Context.Free.Core as X
import Vehicle.Compile.Prelude.Utils as X
import Vehicle.Data.DeBruijn (Ix (..), Lv (..))
import Vehicle.Data.Expr.Standard as X
import Vehicle.Prelude as X
import Vehicle.Prelude.Logging.Class as X
import Vehicle.Resource as X
import Vehicle.Syntax.AST as X
import Vehicle.Syntax.Builtin (Builtin)

--------------------------------------------------------------------------------
-- Type synonyms

type DeclProvenance = (Identifier, Provenance)

--------------------------------------------------------------------------------
-- Other

data Contextualised object context = WithContext
  { objectIn :: object,
    contextOf :: context
  }
  deriving (Show)

type family WithContext a

mapObject :: (a -> b) -> Contextualised a ctx -> Contextualised b ctx
mapObject f WithContext {..} = WithContext {objectIn = f objectIn, ..}

-------------------------------------------------------------------------------
-- Utilities for traversing auxiliary arguments.

type Imports = [Prog Ix Builtin]

mergeImports :: Imports -> Prog Ix Builtin -> Prog Ix Builtin
mergeImports imports userProg = Main $ concatMap (\(Main ds) -> ds) (imports <> [userProg])
