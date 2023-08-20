module Vehicle.Compile.Type.Subsystem.InputOutputInsertion
  ( addFunctionAuxiliaryInputOutputConstraints,
  )
where

import Control.Monad.State (MonadState (..), evalStateT, modify)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Monad (TCM, createFreshInstanceConstraint, freshMetaExpr)
import Vehicle.Expr.BuiltinInterface
import Vehicle.Expr.Normalised
import Vehicle.Syntax.Builtin

-------------------------------------------------------------------------------
-- Inserting polarity and linearity constraints to capture function application

-- | Function for inserting function input and output constraints. Traverses
-- the declaration type, replacing linearity and polarity types with fresh
-- meta variables, and then relates the the two by adding a new suitable
-- constraint.
addFunctionAuxiliaryInputOutputConstraints ::
  (TCM builtin m) =>
  (FunctionPosition -> builtin) ->
  Decl Ix builtin ->
  m (Decl Ix builtin)
addFunctionAuxiliaryInputOutputConstraints mkConstraint = \case
  DefFunction p ident anns t e -> do
    logCompilerPass MaxDetail "insertion of input/output constraints" $ do
      t' <- evalStateT (decomposePiType mkConstraint (ident, p) 0 t) mempty
      return $ DefFunction p ident anns t' e
  d -> return d

decomposePiType ::
  (TCM builtin m, MonadState (MetaMap (Expr Ix builtin)) m) =>
  (FunctionPosition -> builtin) ->
  DeclProvenance ->
  Int ->
  Type Ix builtin ->
  m (Type Ix builtin)
decomposePiType mkConstraint declProv@(ident, p) inputNumber = \case
  Pi p' binder res
    | isExplicit binder -> do
        let position = FunctionInput (nameOf ident) inputNumber
        newType <- addFunctionConstraint mkConstraint (p, position) (typeOf binder)
        let newBinder = replaceBinderType newType binder
        newBody <- decomposePiType mkConstraint declProv (inputNumber + 1) res
        return $ Pi p' newBinder newBody
    | otherwise -> do
        Pi p' binder <$> decomposePiType mkConstraint declProv inputNumber res
  outputType -> do
    -- Reset the state to empty
    modify (const mempty)
    let position = FunctionOutput (nameOf ident)
    addFunctionConstraint mkConstraint (p, position) outputType

addFunctionConstraint ::
  (TCM builtin m, MonadState (MetaMap (Expr Ix builtin)) m) =>
  (FunctionPosition -> builtin) ->
  (Provenance, FunctionPosition) ->
  Expr Ix builtin ->
  m (Expr Ix builtin)
addFunctionConstraint mkConstraint (declProv, position) existingExpr = do
  let p = provenanceOf existingExpr
  newExpr <- case existingExpr of
    Meta _ metaID -> do
      existingSubsitutions <- get
      case MetaMap.lookup metaID existingSubsitutions of
        Nothing -> do
          freshMeta <- unnormalised <$> freshMetaExpr p (TypeUniverse p 0) mempty
          modify (MetaMap.insert metaID freshMeta)
          return freshMeta
        Just existingMeta -> return existingMeta
    _ -> unnormalised <$> freshMetaExpr p (TypeUniverse p 0) mempty

  let constraintArgs =
        RelevantExplicitArg p <$> case position of
          FunctionInput {} -> [newExpr, existingExpr]
          FunctionOutput {} -> [existingExpr, newExpr]
  let tcExpr = BuiltinExpr declProv (mkConstraint position) constraintArgs

  _ <- createFreshInstanceConstraint mempty (existingExpr, mempty, UnitLiteral p) Irrelevant tcExpr

  return newExpr
