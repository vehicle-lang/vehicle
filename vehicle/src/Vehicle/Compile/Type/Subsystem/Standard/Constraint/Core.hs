{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard.Constraint.Core where

import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalised (Value (..))

--------------------------------------------------------------------------------
-- Instance constraints

getTypeClass :: (MonadCompile m) => StandardNormExpr -> m (TypeClass, StandardSpine)
getTypeClass = \case
  VBuiltin (TypeClass tc) args -> return (tc, args)
  _ -> compilerDeveloperError "Unexpected non-type-class instance argument found."
