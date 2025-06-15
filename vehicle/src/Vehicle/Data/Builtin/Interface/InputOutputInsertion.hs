module Vehicle.Data.Builtin.Interface.InputOutputInsertion
  ( addFunctionAuxiliaryInputOutputConstraints,
  )
where

import Control.Monad.State (MonadState (..), evalStateT, modify)
import Vehicle.Compile.Context.Free (getFreeEnv)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core (InstanceConstraintOrigin (..), InstanceTypeRestrictionOrigin (..))
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Monad
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Interface (BuiltinHasStandardData)

-------------------------------------------------------------------------------
-- Inserting polarity and linearity constraints to capture function application

-- | Function for inserting function input and output constraints. Traverses
-- the declaration type, replacing linearity and polarity types with fresh
-- meta variables, and then relates the the two by adding a new suitable
-- constraint.
addFunctionAuxiliaryInputOutputConstraints ::
  (MonadTypeChecker builtin m, BuiltinHasStandardData builtin) =>
  (FunctionPosition -> builtin) ->
  Decl builtin ->
  m (Decl builtin)
addFunctionAuxiliaryInputOutputConstraints mkConstraint = \case
  DefFunction p ident anns t e
    | not (isTypeSynonym t) -> do
        logCompilerPass MaxDetail "insertion of input/output constraints" $ do
          t' <- evalStateT (decomposePiType mkConstraint (ident, p) 0 t) mempty
          return $ DefFunction p ident anns t' e
  d -> return d

decomposePiType ::
  (MonadTypeChecker builtin m, MonadState (MetaMap (Expr builtin)) m, BuiltinHasStandardData builtin) =>
  (FunctionPosition -> builtin) ->
  DeclProvenance ->
  Int ->
  Type builtin ->
  m (Type builtin)
decomposePiType mkConstraint declProv@(ident, _) inputNumber = \case
  Pi p' binder res
    | isExplicit binder -> do
        let position = FunctionInput (nameOf ident) inputNumber
        newType <- addFunctionConstraint (mkConstraint position) declProv position (typeOf binder)
        let newBinder = replaceBinderType newType binder
        newBody <- decomposePiType mkConstraint declProv (inputNumber + 1) res
        return $ Pi p' newBinder newBody
    | otherwise -> do
        Pi p' binder <$> decomposePiType mkConstraint declProv inputNumber res
  outputType -> do
    -- Reset the state to empty
    modify (const mempty)
    let position = FunctionOutput (nameOf ident)
    addFunctionConstraint (mkConstraint position) declProv position outputType

addFunctionConstraint ::
  (MonadTypeChecker builtin m, MonadState (MetaMap (Expr builtin)) m, BuiltinHasStandardData builtin) =>
  builtin ->
  DeclProvenance ->
  FunctionPosition ->
  Expr builtin ->
  m (Expr builtin)
addFunctionConstraint constraint declProv@(_, declP) position existingExpr = do
  let p = provenanceOf existingExpr
  newExpr <- case existingExpr of
    Meta _ metaID -> do
      existingSubsitutions <- get
      case MetaMap.lookup metaID existingSubsitutions of
        Nothing -> do
          meta <- freshMetaExpr p (TypeUniverse p 0) mempty
          modify (MetaMap.insert metaID meta)
          return meta
        Just existingMeta -> return existingMeta
    _ -> freshMetaExpr p (TypeUniverse p 0) mempty

  let constraintArgs = case position of
        FunctionInput {} -> [newExpr, existingExpr]
        FunctionOutput {} -> [existingExpr, newExpr]
  let tcExpr = BuiltinExpr declP constraint (explicit <$> constraintArgs)

  freeEnv <- getFreeEnv
  let declSort = developerError "function IO constraints should never fail"
  let origin = InstanceTypeRestrictionOrigin $ TypeRestrictionOrigin freeEnv declProv declSort existingExpr
  _ <- createFreshInstanceConstraint True mempty declP origin Irrelevant tcExpr

  return newExpr
