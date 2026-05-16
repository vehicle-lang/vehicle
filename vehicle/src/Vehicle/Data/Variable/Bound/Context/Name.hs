module Vehicle.Data.Variable.Bound.Context.Name
  ( module Export,
    prettyExternalInCtx,
    prettyFriendlyInCtx,
    debugFriendly,
    getFreshTensorBinderName
  )
where

-- Simple module that specialises MonadBoundContext for the common occurence
-- where you only need to know the bound variable's names.

import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.Variable.Bound.Context.Name.Class as Export
import Vehicle.Data.Variable.Bound.Context.Name.Core as Export
import Vehicle.Data.Variable.Bound.Context.Name.Instance as Export
import qualified Data.Text as Text

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

-- Generates an unused binder name of the form '_tN', where N is an integer
getFreshTensorBinderName ::
  NamedBoundCtx ->
  Text.Text
getFreshTensorBinderName ctx = checkExistsInCtx 0
  where
    checkExistsInCtx :: Int -> Text.Text
    checkExistsInCtx n =
      let name = "_t" <> Text.pack (show n)
       in if Just name `elem` ctx
            then checkExistsInCtx (n + 1)
            else "_t" <> Text.pack (show n)