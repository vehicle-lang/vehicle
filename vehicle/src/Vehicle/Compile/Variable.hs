module Vehicle.Compile.Variable
  ( createUserVar,
  )
where

-- Needed as Applicative is exported by Prelude in GHC 9.6 and above.
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value (VBinder, Value)
import Prelude hiding (Applicative (..))

--------------------------------------------------------------------------------
-- Extraction

type MonadCreateUserVar m =
  ( MonadCompile m
  )

createUserVar ::
  (MonadCreateUserVar m) =>
  DeclProvenance ->
  NamedBoundCtx ->
  VBinder Builtin ->
  m (Name, Value Builtin)
createUserVar propertyProvenance namedCtx binder = do
  let varName = getBinderName binder
  checkUserVariableNameIsUnique propertyProvenance namedCtx varName
  varDimensions <- checkUserVariableType binder
  return (varName, varDimensions)

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
  VBinder Builtin ->
  m (Value Builtin)
checkUserVariableType binder =
  case toTypeValue (typeOf binder) of
    VRatTensorType dims -> return dims
    _ -> developerError $ "Unexpected quantifier type:" <+> prettyVerbose (typeOf binder)
