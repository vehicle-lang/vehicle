{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Vehicle.Core.Check.Core where

import Control.Exception (Exception)
import Control.Monad.Except (MonadError)
import Vehicle.Prelude

-- | Errors that may arise during checking.
data CheckError
  = UnknownBuiltin Token
  deriving (Show)

instance Exception CheckError

-- | Constraint for the monad stack used by the checkers.
type MonadCheck m = MonadError CheckError m
