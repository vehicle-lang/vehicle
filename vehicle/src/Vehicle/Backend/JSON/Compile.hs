module Vehicle.Backend.JSON.Compile
  ( compileProgToJSON,
  )
where

import Data.Text (Text)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Type.Subsystem.Standard (StandardGluedProg)

compileProgToJSON :: (MonadCompile m) => StandardGluedProg -> m Text
compileProgToJSON _ = return "<placeholder>"
