module Vehicle.Compile.Variable
  ( createUserVar,
  )
where

-- Needed as Applicative is exported by Prelude in GHC 9.6 and above.
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print.Builtin
import Vehicle.Data.Builtin.Interface (BuiltinHasNatLiterals, BuiltinHasRatType (mkRatBuiltinType), BuiltinHasVecType)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value (VBinder, Value)
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (TensorShape)
import Prelude hiding (Applicative (..))

--------------------------------------------------------------------------------
-- Extraction

type MonadCreateUserVar builtin m =
  ( MonadCompile m,
    BuiltinHasRatType builtin,
    BuiltinHasVecType builtin,
    BuiltinHasNatLiterals builtin,
    PrintableBuiltin builtin,
    Eq builtin
  )

createUserVar ::
  (MonadCreateUserVar builtin m) =>
  DeclProvenance ->
  NamedBoundCtx ->
  VBinder builtin ->
  m (TensorVariable, TensorShape)
createUserVar propertyProvenance namedCtx binder = do
  let varName = getBinderName binder
  checkUserVariableNameIsUnique propertyProvenance namedCtx varName
  varDimensions <- checkUserVariableType propertyProvenance binder
  return (makeTensorVariable varName, varDimensions)

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
  forall m builtin.
  (MonadCreateUserVar builtin m) =>
  DeclProvenance ->
  VBinder builtin ->
  m TensorShape
checkUserVariableType propertyProvenance binder = go (typeOf binder)
  where
    go :: Value builtin -> m TensorShape
    go = \case
      IRatType {} -> return []
      IVectorType _ tElem (INatLiteral _ d) -> do
        ds <- go tElem
        return $ d : ds
      tElem -> do
        let p = provenanceOf binder
        let baseName = getBinderName binder
        throwError $ UnsupportedVariableType propertyProvenance p baseName tElem (typeOf binder) [mkRatBuiltinType]
