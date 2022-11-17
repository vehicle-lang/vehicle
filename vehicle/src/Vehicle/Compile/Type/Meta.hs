module Vehicle.Compile.Type.Meta
  ( MetaSet
  , MetaInfo(..)
  , increaseMetaCtxSize
  , HasMetas(..)
  , makeMetaType
  , makeMetaExpr
  , getMetaDependencies
  , getNormMetaDependencies
  ) where

import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Variable
