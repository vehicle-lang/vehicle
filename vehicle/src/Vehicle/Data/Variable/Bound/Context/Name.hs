module Vehicle.Data.Variable.Bound.Context.Name
  ( module Export,
    prettyExternalInCtx,
    prettyFriendlyInCtx,
    debugFriendly,
  )
where

-- Simple module that specialises MonadBoundContext for the common occurence
-- where you only need to know the bound variable's names.

import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.Variable.Bound.Context.Name.Class as Export
import Vehicle.Data.Variable.Bound.Context.Name.Core as Export
import Vehicle.Data.Variable.Bound.Context.Name.Instance as Export

prettyFriendlyInCtx ::
  (MonadReadableNameContext m, PrettyFriendly (Contextualised a NamedBoundCtx)) =>
  a ->
  m (Doc b)
prettyFriendlyInCtx value = prettyFriendly . WithContext value <$> getNameContext

prettyExternalInCtx ::
  (MonadReadableNameContext m, PrettyExternal (Contextualised a NamedBoundCtx)) =>
  a ->
  m (Doc b)
prettyExternalInCtx e = prettyExternal . WithContext e <$> getNameContext

debugFriendly :: (MonadReadableNameContext m, PrettyFriendly (Contextualised a NamedBoundCtx), MonadLogger m) => a -> m ()
debugFriendly value = logDebugM MaxDetail $ prettyFriendlyInCtx value
