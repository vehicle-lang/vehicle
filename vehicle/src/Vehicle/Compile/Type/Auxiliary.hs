module Vehicle.Compile.Type.Auxiliary
  ( insertHolesForAuxiliaryAnnotations,
    addFunctionAuxiliaryInputOutputConstraints,
  )
where

import Control.Monad.State (MonadState (..), evalStateT, modify)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Vehicle.Compile.Error (compilerDeveloperError)
import Vehicle.Compile.Normalise as OldNorm
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.VariableContext (toNormalisationDeclContext)
import Vehicle.Expr.Normalised

-------------------------------------------------------------------------------
-- Utilities for traversing auxiliary arguments.

data AuxType = Pol | Lin

-- | Function for updating an auxiliary argument (which may be missing)
type AuxArgUpdate m = Provenance -> AuxType -> Maybe (CheckedExpr Builtin) -> m (CheckedExpr Builtin)

class TraverseAuxiliaryArguments a where
  -- | Traverses all the auxiliary type arguments in the provided element,
  -- applying the provided update function when it finds them (or a space
  -- where they should be).
  traverseAux :: TCM Builtin m => AuxArgUpdate m -> a -> m a

instance TraverseAuxiliaryArguments (UncheckedExpr Builtin) where
  traverseAux f expr = case expr of
    BoolType p -> do
      lin <- f p Lin Nothing
      pol <- f p Pol Nothing
      return $ mkAnnBoolType p lin pol
    AnnBoolType p lin pol -> do
      lin' <- f p Lin (Just lin)
      pol' <- f p Pol (Just pol)
      return $ mkAnnBoolType p lin' pol'
    RatType p -> do
      lin <- f p Lin Nothing
      return $ mkAnnRatType p lin
    AnnRatType p lin -> do
      lin' <- f p Lin (Just lin)
      return $ mkAnnRatType p lin'
    FreeVar p ident ->
      traverseFreeVariable f p ident []
    App p (FreeVar _ ident) args -> do
      traverseFreeVariable f p ident (NonEmpty.toList args)
    Ann p e t -> Ann p <$> traverseAux f e <*> traverseAux f t
    App p fun args -> App p <$> traverseAux f fun <*> traverseAux f args
    Pi p binder res -> Pi p <$> traverseAux f binder <*> traverseAux f res
    Let p bound binder body -> Let p <$> traverseAux f bound <*> traverseAux f binder <*> traverseAux f body
    Lam p binder body -> Lam p <$> traverseAux f binder <*> traverseAux f body
    LVec p xs -> LVec p <$> traverse (traverseAux f) xs
    Universe {} -> return expr
    Var {} -> return expr
    Hole {} -> return expr
    Meta {} -> return expr
    Literal {} -> return expr
    Builtin {} -> return expr

instance TraverseAuxiliaryArguments (UncheckedArg Builtin) where
  traverseAux f = traverse (traverseAux f)

instance TraverseAuxiliaryArguments (UncheckedBinder Builtin) where
  traverseAux f = traverse (traverseAux f)

instance TraverseAuxiliaryArguments a => TraverseAuxiliaryArguments (NonEmpty a) where
  traverseAux f = traverse (traverseAux f)

instance TraverseAuxiliaryArguments a => TraverseAuxiliaryArguments [a] where
  traverseAux f = traverse (traverseAux f)

mkAnnBoolType :: Provenance -> CheckedExpr Builtin -> CheckedExpr Builtin -> CheckedExpr Builtin
mkAnnBoolType p lin pol =
  ConstructorExpr
    p
    Bool
    [ IrrelevantImplicitArg p lin,
      IrrelevantImplicitArg p pol
    ]

mkAnnRatType :: Provenance -> CheckedExpr Builtin -> CheckedExpr Builtin
mkAnnRatType p lin = ConstructorExpr p Rat [IrrelevantImplicitArg p lin]

traverseFreeVariable :: TCM Builtin m => AuxArgUpdate m -> Provenance -> Identifier -> [CheckedArg Builtin] -> m (CheckedExpr Builtin)
traverseFreeVariable f p ident args = do
  args' <- traverseAux f args
  declType <- getDeclType p ident
  args'' <-
    if isTypeSynonym declType
      then traverseAuxFreeVarArgs f p declType args'
      else return args'

  return $ normAppList p (FreeVar p ident) args''

-- | Traverses a telescope of the expected type and the actual provided arguments
-- applying the update function everywhere it finds an auxiliary argument in the type.
traverseAuxFreeVarArgs ::
  TCM Builtin m =>
  AuxArgUpdate m ->
  Provenance ->
  CheckedType Builtin ->
  [CheckedArg Builtin] ->
  m [CheckedArg Builtin]
traverseAuxFreeVarArgs f p declType declArgs = case (declType, declArgs) of
  (Pi _ binder res, arg : args) -> do
    let inputType = binderType binder
    args' <- traverseAuxFreeVarArgs f p res args
    arg' <-
      if isPolarityUniverse inputType
        then traverse (\e -> f (provenanceOf e) Pol (Just e)) arg
        else
          if isLinearityUniverse inputType
            then traverse (\e -> f (provenanceOf e) Lin (Just e)) arg
            else return arg
    return (arg' : args')
  (Pi _ binder res, [])
    | isImplicit binder && isAuxiliaryUniverse (typeOf binder) -> do
        xs <- traverseAuxFreeVarArgs f p res []
        meta <- case binderType binder of
          LinearityUniverse {} -> f p Lin Nothing
          PolarityUniverse {} -> f p Pol Nothing
          _ -> compilerDeveloperError "Mismatch between cases and 'isAuxiliaryUniverse'"
        return $ IrrelevantImplicitArg p meta : xs
  (_, []) -> return []
  (FreeVar {}, _) -> do
    normDeclType <- oldNorm declType
    traverseAuxFreeVarArgs f p normDeclType declArgs
  (App _ FreeVar {} _, _) -> do
    normDeclType <- oldNorm declType
    traverseAuxFreeVarArgs f p normDeclType declArgs
  (_, _) ->
    compilerDeveloperError $
      "Malformed type and arguments found when traversing auxilarity arguments"
        <> line
        <> indent
          2
          ( "Type:"
              <+> prettyVerbose declType
                <> line
                <> "Args:"
              <+> prettyVerbose declArgs
          )

oldNorm :: MonadTypeChecker Builtin m => TypeCheckedExpr -> m TypeCheckedExpr
oldNorm e = do
  declCtx <- getDeclContext
  runSilentLoggerT $
    normaliseExpr
      e
      Options
        { declContext = toNormalisationDeclContext declCtx,
          boundContext = mempty, -- see issue #129
          normaliseDeclApplications = True,
          normaliseLambdaApplications = True,
          normaliseStdLibApplications = True,
          normaliseBuiltin = const True
        }

-------------------------------------------------------------------------------
-- Inserting polarity and linearity annotations

-- | Inserts holes for all the non-user facing auxilliary annotations, e.g.
-- polarity annotations on the `Bool` type and linearity annotations on the
-- `Rat` type.
insertHolesForAuxiliaryAnnotations ::
  TCM Builtin m => UncheckedExpr Builtin -> m (UncheckedExpr Builtin)
insertHolesForAuxiliaryAnnotations e =
  logCompilerPass MaxDetail "insertion of missing auxiliary types" $
    traverseAux insertionUpdateFn e

insertionUpdateFn :: TCM Builtin m => AuxArgUpdate m
insertionUpdateFn p auxType = \case
  Just e -> return e
  Nothing -> case auxType of
    Lin -> unnormalised <$> freshLinearityMeta p
    Pol -> unnormalised <$> freshPolarityMeta p

-------------------------------------------------------------------------------
-- Inserting polarity and linearity constraints to capture function application

-- | Function for inserting function input and output constraints. Traverses
-- the declaration type, replacing linearity and polarity types with fresh
-- meta variables, and then relates the the two by adding a new suitable
-- constraint.
addFunctionAuxiliaryInputOutputConstraints ::
  TCM Builtin m =>
  CheckedDecl Builtin ->
  m (CheckedDecl Builtin)
addFunctionAuxiliaryInputOutputConstraints = \case
  DefFunction p ident isProperty t e -> do
    logCompilerPass MaxDetail "insertion of function constraints" $ do
      t' <- evalStateT (decomposePiType (ident, p) 0 t) mempty
      return $ DefFunction p ident isProperty t' e
  d -> return d

decomposePiType ::
  (TCM Builtin m, MonadState (MetaMap (CheckedExpr Builtin)) m) =>
  DeclProvenance ->
  Int ->
  CheckedType Builtin ->
  m (CheckedType Builtin)
decomposePiType declProv@(ident, p) explicitBindingDepth = \case
  Pi p' binder res
    | isExplicit binder -> do
        let position = FunctionInput (nameOf ident) explicitBindingDepth
        newBinder <- traverseAux (replaceAux (p, position)) binder
        Pi p' newBinder <$> decomposePiType declProv (explicitBindingDepth + 1) res
    | otherwise -> do
        Pi p' binder <$> decomposePiType declProv explicitBindingDepth res
  outputType -> do
    -- Reset the state to empty
    modify (const mempty)
    let position = FunctionOutput (nameOf ident)
    traverseAux (replaceAux (p, position)) outputType

replaceAux ::
  (TCM Builtin m, MonadState (MetaMap (CheckedExpr Builtin)) m) =>
  (Provenance, FunctionPosition) ->
  AuxArgUpdate m
replaceAux position p auxType = \case
  Nothing ->
    compilerDeveloperError
      "Should not be missing auxiliary arguments during function constraint insertion"
  Just expr -> case auxType of
    Lin -> addFunctionConstraint (LinearityTypeClass . FunctionLinearity) (unnormalised <$> freshLinearityMeta p) position expr
    Pol -> addFunctionConstraint (PolarityTypeClass . FunctionPolarity) (unnormalised <$> freshPolarityMeta p) position expr

addFunctionConstraint ::
  (TCM Builtin m, MonadState (MetaMap (CheckedExpr Builtin)) m) =>
  (FunctionPosition -> TypeClass) ->
  m (CheckedExpr Builtin) ->
  (Provenance, FunctionPosition) ->
  CheckedExpr Builtin ->
  m (CheckedExpr Builtin)
addFunctionConstraint mkTC createNewMeta (declProv, position) existingExpr = do
  newExpr <- case existingExpr of
    Meta _ m -> do
      xs <- get
      case MetaMap.lookup m xs of
        Nothing -> do
          m' <- createNewMeta
          modify (MetaMap.insert m m')
          return m'
        Just e -> return e
    _ -> createNewMeta

  let constraintArgs =
        ExplicitArg (provenanceOf existingExpr) <$> case position of
          FunctionInput {} -> [newExpr, existingExpr]
          FunctionOutput {} -> [existingExpr, newExpr]
  let tcExpr = BuiltinTypeClass declProv (mkTC position) constraintArgs

  _ <- createFreshTypeClassConstraint mempty (existingExpr, mempty) tcExpr

  return newExpr
