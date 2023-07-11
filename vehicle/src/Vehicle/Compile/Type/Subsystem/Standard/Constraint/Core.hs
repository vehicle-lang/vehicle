{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard.Constraint.Core where

import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..))
import Vehicle.Expr.Normalised (Value (..))

--------------------------------------------------------------------------------
-- Instance constraints

type MonadInstance m = TCM StandardBuiltin m

getTypeClass :: (MonadCompile m) => StandardNormExpr -> m (TypeClass, StandardExplicitSpine)
getTypeClass = \case
  (VBuiltin (CType (StandardTypeClass tc)) args) -> return (tc, args)
  _ -> compilerDeveloperError "Unexpected non-type-class instance argument found."

relevanceOfTypeClass :: (MonadCompile m) => StandardNormType -> m Relevance
relevanceOfTypeClass t = do
  (tc, _) <- getTypeClass t
  return $ relevanceOf tc
