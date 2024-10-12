module Vehicle.Compile.Variable
  ( createUserVar,
  )
where

-- Needed as Applicative is exported by Prelude in GHC 9.6 and above.
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (QuoteClosure)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print.Builtin
import Vehicle.Data.Builtin.Interface (BuiltinHasNatLiterals, BuiltinHasRatType (mkRatBuiltinType), BuiltinHasVecType)
import Vehicle.Data.Builtin.Standard (Builtin)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value (VBinder, Value)
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (TensorShape)
import Prelude hiding (Applicative (..))

--------------------------------------------------------------------------------
-- Extraction

type MonadCreateUserVar closure builtin m =
  ( MonadCompile m,
    BuiltinHasRatType builtin,
    BuiltinHasVecType builtin,
    BuiltinHasNatLiterals builtin,
    PrintableBuiltin builtin,
    Show closure,
    Eq closure,
    Eq builtin,
    QuoteClosure closure Builtin
  )

createUserVar ::
  (MonadCreateUserVar closure builtin m) =>
  DeclProvenance ->
  NamedBoundCtx ->
  VBinder closure builtin ->
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
  forall m closure builtin.
  (MonadCreateUserVar closure builtin m) =>
  DeclProvenance ->
  VBinder closure builtin ->
  m TensorShape
checkUserVariableType propertyProvenance binder = go (typeOf binder)
  where
    go :: Value closure builtin -> m TensorShape
    go = \case
      IRatType {} -> return []
      IVectorType _ tElem (INatLiteral _ d) -> do
        ds <- go tElem
        return $ d : ds
      tElem -> do
        let p = provenanceOf binder
        let baseName = getBinderName binder
        throwError $ UnsupportedVariableType propertyProvenance p baseName tElem (typeOf binder) [mkRatBuiltinType]
