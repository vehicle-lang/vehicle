{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard.Constraint.Core where

import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Core

--------------------------------------------------------------------------------
-- Instance constraints

type MonadInstance m = TCM StandardBuiltinType m

getTypeClass :: (MonadCompile m) => StandardBuiltinType -> m TypeClass
getTypeClass = \case
  StandardTypeClass tc -> return tc
  _ -> compilerDeveloperError "Unexpected non-type-class instance argument found."
