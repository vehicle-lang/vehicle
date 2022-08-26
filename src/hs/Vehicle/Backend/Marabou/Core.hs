module Vehicle.Backend.Marabou.Core
  ( module Vehicle.Backend.Marabou.Core
  , Property(..)
  , PropertyState(..)
  ) where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource (MetaNetwork)
import Vehicle.Compile.Linearity.Core (UserVarReconstructionInfo)

type MarabouSpec = [(Symbol, MarabouProperty)]

type MarabouProperty = Property MarabouQuery

data MarabouQuery = MarabouQuery
  { doc               :: Doc ()
  , vars              :: [Symbol]
  , metaNetwork       :: MetaNetwork
  , varReconstruction :: UserVarReconstructionInfo
  }