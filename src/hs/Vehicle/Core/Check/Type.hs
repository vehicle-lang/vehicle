{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}

module Vehicle.Core.Check.Type where

import           Control.Exception (Exception)
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.State (MonadState(..))
import           Data.Functor.Foldable (fold)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Vehicle.Core.Type
import qualified Vehicle.Core.Abs as VCA (SortedName(..), Name(..))
import           Vehicle.Prelude

data TypeError
  = UnboundName Token
  deriving (Show)
