{-# HLINT ignore "Use <|>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Type.Force
  ( forceThunkWithMetas,
    forceApplicationWithMetas,
  )
where

import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Control.Monad.Writer.Strict
import Vehicle.Compile.Normalise.Core (NormalisableBuiltin)
import Vehicle.Compile.Normalise.NBEForced
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta (MetaInfo (..), MetaSet, MetaVariableContext, findMetaInfo)
import qualified Vehicle.Compile.Type.Meta.Set as MetaSet
import Vehicle.Data.Code.ForcedValue (ForcedValueWithMetas, GenericForcedValue (..), ThunkWithMetas, UnforcedSpineWithMetas, normalised)
import Vehicle.Data.Variable.Bound.Context.Name (runNameBoundContextT)
import Vehicle.Data.Variable.Bound.Context.Name.Core (NamedBoundCtx)
import Vehicle.Data.Variable.Free.Context.Class (MonadFreeContext)
