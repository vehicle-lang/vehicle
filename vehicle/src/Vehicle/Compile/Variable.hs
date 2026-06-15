module Vehicle.Compile.Variable
  ( createUserVar,
  )
where

-- Needed as Applicative is exported by Prelude in GHC 9.6 and above.
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBEForced (MonadNorm, forceThunk)
import Vehicle.Compile.Normalise.TypedValueForced
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Variable.Bound.Context.Name
import Prelude hiding (Applicative (..))

--------------------------------------------------------------------------------
-- Extraction

type MonadCreateUserVar m =
  (MonadCompile m, MonadNorm Builtin m)

createUserVar ::
  (MonadCreateUserVar m) =>
  DeclProvenance ->
  NamedBoundCtx ->
  UnforcedBinder Builtin ->
  m (UnforcedDims Builtin)
createUserVar propertyProvenance namedCtx binder = do
  let varName = getBinderName binder
  checkUserVariableNameIsUnique propertyProvenance namedCtx varName
  varDimensions <- checkUserVariableType binder
  return varDimensions

checkUserVariableNameIsUnique ::
  (MonadCompile m) =>
  DeclProvenance ->
  NamedBoundCtx ->
  Name ->
  m ()
checkUserVariableNameIsUnique propertyProvenance namedCtx varName = do
  let isDuplicateName = Just varName `elem` namedCtx
  when isDuplicateName $
    throwError $
      DuplicateQuantifierNames propertyProvenance varName

checkUserVariableType ::
  forall m.
  (MonadCreateUserVar m) =>
  UnforcedBinder Builtin ->
  m (Thunk Builtin)
checkUserVariableType binder = do
  forcedType <- forceThunk (typeOf binder)
  case toTypeValue forcedType of
    VTensorType _ dims -> return dims
    _ -> developerError $ "Unexpected quantifier type:" <+> prettyVerbose (typeOf binder)
