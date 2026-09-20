module Vehicle.Compile.Constants.Atom.Core where

import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.ForcedValue

-----------------------------------------------------------------------------
-- Atoms

-- | A tensor-valued term that a linear expression treats as opaque.
data Atom = Atom
  { atomDims :: UnforcedDims Builtin,
    atomBody :: Thunk Builtin
  }
  deriving (Eq, Ord, Show)
