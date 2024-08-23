module Vehicle.Compile.Variable
  ( createUserVar,
  )
where

-- Needed as Applicative is exported by Prelude in GHC 9.6 and above.
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Expr.Interface
import Vehicle.Data.Expr.Value
import Vehicle.Data.QuantifiedVariable
import Prelude hiding (Applicative (..))

--------------------------------------------------------------------------------
-- Extraction

createUserVar ::
  (MonadCompile m) =>
  DeclProvenance ->
  NamedBoundCtx ->
  WHNFBinder Builtin ->
  m OriginalUserVariable
createUserVar propertyProvenance namedCtx binder = do
  let varName = getBinderName binder
  checkUserVariableNameIsUnique propertyProvenance namedCtx varName
  varDimensions <- checkUserVariableType propertyProvenance binder
  return $
    OriginalUserVariable
      { userTensorVarName = varName,
        userTensorVarDimensions = varDimensions
      }

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
  (MonadCompile m) =>
  DeclProvenance ->
  WHNFBinder Builtin ->
  m TensorShape
checkUserVariableType propertyProvenance binder = go (typeOf binder)
  where
    go :: WHNFType Builtin -> m TensorShape
    go = \case
      IRatType {} -> return []
      IVectorType _ tElem (INatLiteral _ d) -> do
        ds <- go tElem
        return $ d : ds
      tElem -> do
        let p = provenanceOf binder
        let baseName = getBinderName binder
        throwError $ UnsupportedVariableType propertyProvenance p baseName tElem (typeOf binder) [BuiltinType Rat]
