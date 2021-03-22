{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Vehicle.Core.Check.Core where

import           Control.Exception (Exception)
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.State (MonadState(..))
import           Data.Text (Text)
import           Text.Printf (printf)
import           Vehicle.Core.Type
import           Vehicle.Prelude


-- |Errors that may arise during type checking.
data TypeError
  = UnknownBuiltin Token
  | UnboundName Token
  | UnexpectedName Token
  | UnexpectedSort Sort
  deriving Show

instance Exception TypeError

-- |Constraint for the monad stack used by type checkers.
type TCM m = (MonadError TypeError m)
