{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Prelude
  ( module X,
    module Vehicle.Compile.Prelude,
    Ix (..),
    Lv (..),
  )
where

import Vehicle.Compile.Prelude.Utils as X
import Vehicle.Data.Code.Expr as X
import Vehicle.Data.Code.Prog as X
import Vehicle.Data.Variable.Bound.Context.Core as X
import Vehicle.Data.Variable.Bound.Index (Ix (..))
import Vehicle.Data.Variable.Bound.Level (Lv (..))
import Vehicle.Data.Variable.Free.Context.Core as X
import Vehicle.Prelude as X
import Vehicle.Prelude.Logging.Class as X
import Vehicle.Resource as X

--------------------------------------------------------------------------------
-- Type synonyms

type DeclProvenance = (Identifier, Provenance)

instance HasName DeclProvenance Name where
  nameOf = nameOf . fst

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

mapContextOf :: (ctx1 -> ctx2) -> Contextualised object ctx1 -> Contextualised object ctx2
mapContextOf f WithContext {..} = WithContext {contextOf = f contextOf, ..}

getTensorRecordFields :: Expr builtin -> RecordFields builtin
getTensorRecordFields = \case
  Record _ _ fields -> fields
  _ -> developerError "Malformed tensor record definition"
