module Vehicle.Backend.Marabou.Core where

import Vehicle.Prelude
import Vehicle.Compile.Normalise.NetworkApplications (MetaNetwork)

data MarabouProperty = MarabouProperty
  { name    :: Symbol
  -- Did the property contain universal quantifier and is therefore negated?
  , negated :: Bool
  -- The individual queries (disjuncts) needed to prove the property
  , queries :: [MarabouQuery]
  }

data MarabouQuery = MarabouQuery
  { doc         :: Doc ()
  , vars        :: [MarabouVar]
  , metaNetwork :: MetaNetwork
  }

data MarabouVar = MarabouVar
  { name    :: Symbol         -- Name of the variable
  , varType :: MarabouVarType -- Type of the variable
  }

data MarabouVarType
  = MReal
