{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}

module Vehicle.Core.Check.Core where

import           Control.Exception (Exception)
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.State (MonadState(..))
import           Data.Text (Text)
import           Vehicle.Core.Type
import           Vehicle.Prelude


-- |Errors that may arise during type checking.
data TypeError
  = UnknownBuiltin Token
  | UnboundName Token
  deriving (Show)

instance Exception TypeError

-- |Constraint for the monad stack used by type checkers.
type TCM m = (MonadError TypeError m)
