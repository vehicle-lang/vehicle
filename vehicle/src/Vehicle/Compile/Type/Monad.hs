module Vehicle.Compile.Type.Monad
  ( module Vehicle.Compile.Type.Monad.Class
  , module Vehicle.Compile.Type.Monad.Instance
  , TCM
  ) where

import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.Monad.Instance

-- | The type-checking monad.
type TCM m = MonadTypeChecker m
