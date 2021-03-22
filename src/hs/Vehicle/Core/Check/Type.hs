{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeFamilies       #-}

module Vehicle.Core.Check.Type where

import           Control.Exception (Exception)
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.State (MonadState(..))
import           Data.Functor.Foldable (fold)
import           Data.Text (Text)
import           Vehicle.Core.Check.Core
import           Vehicle.Core.Check.Builtin
import           Vehicle.Core.Type
import           Vehicle.Core.Abs (SortedName(..), Name(..))
import           Vehicle.Prelude

type KindEnv name builtin ann = [(Text, Kind name builtin ann)]
type TypeEnv name builtin ann = [(Text, Type name builtin ann)]

data DeBruijn (sort :: Sort) = DeBruijn
  { pos   :: Position
  , index :: Int
  }

{-
-- | Annotation which stores typing information.
type family CheckInfo (sort :: Sort) where
  CheckInfo 'TYPE = Kind DeBruijn Builtin NoAnn
  CheckInfo 'EXPR = Type DeBruijn Builtin NoAnn
  CheckInfo 'TARG = Kind DeBruijn Builtin NoAnn
  CheckInfo 'EARG = Type DeBruijn Builtin NoAnn
  CheckInfo _     = ()

newtype CheckResult (sort :: Sort) = CheckResult (CheckInfo sort)

{-
class Check (tree :: (Sort -> *) -> (Sort -> *) -> (Sort -> *) -> *) where
  check :: TCM m => CheckResult -> tree SortedName Builtin NoAnn -> m (tree DeBruijn Builtin CheckResult)

class Infer (tree :: (Sort -> *) -> (Sort -> *) -> (Sort -> *) -> *) where
  infer :: TCM m => tree SortedName Builtin NoAnn -> m (tree DeBruijn Builtin CheckResult)
-}

-- -}
-- -}
-- -}
-- -}
-- -}
