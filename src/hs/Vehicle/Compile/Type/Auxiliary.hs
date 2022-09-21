module Vehicle.Compile.Type.Auxiliary
  ( insertHolesForAuxiliaryAnnotations
  , addFunctionAuxiliaryInputOutputConstraints
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error (compilerDeveloperError)
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.WeakHeadNormalForm (whnf)
import Vehicle.Language.Print (prettyVerbose)

-------------------------------------------------------------------------------
-- Utilities for traversing auxiliary arguments.

data AuxType = Pol | Lin

-- | Function for updating an auxiliary argument (which may be missing)
type AuxArgUpdate m = Provenance -> AuxType -> Maybe CheckedExpr -> m CheckedExpr

class TraverseAuxiliaryArguments a where
  -- | Traverses all the auxiliary type arguments in the provided element,
  -- applying the provided update function when it finds them (or a space
  -- where they should be).
  traverseAux :: TCM m => AuxArgUpdate m -> a -> m a

instance TraverseAuxiliaryArguments UncheckedDecl where
  traverseAux f = traverseDeclExprs (traverseAux f)

instance TraverseAuxiliaryArguments UncheckedExpr where
  traverseAux f expr = case expr of
    BoolType p -> do
      lin <- f p Lin Nothing
      pol <- f p Pol Nothing
      return $ AnnBoolType p lin pol

    AnnBoolType p lin pol -> do
      lin' <- f p Lin (Just lin)
      pol' <- f p Pol (Just pol)
      return $ AnnBoolType p lin' pol'

    RatType p -> do
      lin <- f p Lin Nothing
      return $ AnnRatType p lin

    AnnRatType p lin -> do
      lin' <- f p Lin (Just lin)
      return $ AnnRatType p lin'

    FreeVar p ident ->
      traverseFreeVariable f p ident []

    App p (FreeVar _ ident) args ->  do
      traverseFreeVariable f p ident (NonEmpty.toList args)

    Ann  p e t               -> Ann p <$> traverseAux f e <*> traverseAux f t
    App  p fun args          -> App p <$> traverseAux f fun <*> traverseAux f args
    Pi   p binder res        -> Pi  p <$> traverseAux f binder <*> traverseAux f res
    Let  p bound binder body -> Let p <$> traverseAux f bound <*> traverseAux f binder <*> traverseAux f body
    Lam  p binder body       -> Lam p <$> traverseAux f binder <*> traverseAux f body
    LVec p xs                -> LVec p <$> traverse (traverseAux f) xs

    Universe{} -> return expr
    Var{}      -> return expr
    Hole{}     -> return expr
    Meta{}     -> return expr
    Literal{}  -> return expr
    Builtin{}  -> return expr

instance TraverseAuxiliaryArguments UncheckedArg where
  traverseAux f = traverseArgExpr (traverseAux f)

instance TraverseAuxiliaryArguments UncheckedBinder where
  traverseAux f = traverseBinderType (traverseAux f)

instance TraverseAuxiliaryArguments a => TraverseAuxiliaryArguments (NonEmpty a) where
  traverseAux f = traverse (traverseAux f)

instance TraverseAuxiliaryArguments a => TraverseAuxiliaryArguments [a] where
  traverseAux f = traverse (traverseAux f)

traverseFreeVariable :: TCM m => AuxArgUpdate m -> Provenance -> Identifier -> [CheckedArg] -> m CheckedExpr
traverseFreeVariable f p ident args = do
  args' <- traverseAux f args
  declType <- getDeclType p ident
  args'' <- if isTypeSynonym declType
    then traverseAuxFreeVarArgs f p declType args'
    else return args'

  return $ normAppList p (FreeVar p ident) args''

traverseAuxFreeVarArgs :: TCM m
                       => AuxArgUpdate m
                       -> Provenance
                       -> CheckedType
                       -> [CheckedArg]
                       -> m [CheckedArg]
traverseAuxFreeVarArgs f p declType declArgs = case (declType, declArgs) of
  (Pi _ binder res, arg : args) -> do
    args' <- traverseAuxFreeVarArgs f p res args
    arg' <-
      if isPolarityUniverse  (typeOf binder)
        then traverseArgExpr (\e -> f (provenanceOf e) Pol (Just e)) arg
      else if isLinearityUniverse (typeOf binder)
        then traverseArgExpr (\e -> f (provenanceOf e) Lin (Just e)) arg
      else return arg
    return (arg' : args')

  (Pi _ binder res, [])
    | visibilityOf binder == Implicit && isAuxiliaryUniverse (typeOf binder) -> do
    xs <- traverseAuxFreeVarArgs f p res []
    meta <- case typeOf binder of
      LinearityUniverse{} -> f p Lin Nothing
      PolarityUniverse{}  -> f p Pol Nothing
      _                   -> compilerDeveloperError "Mismatch between cases and 'isAuxiliaryUniverse'"
    return $ IrrelevantImplicitArg p meta : xs

  (_, []) -> return []

  (FreeVar{}, _) -> do
    declCtx <- getDeclContext
    normDeclType <- whnf declCtx declType
    traverseAuxFreeVarArgs f p normDeclType declArgs

  (App _ (FreeVar{}) _, _) -> do
    declCtx <- getDeclContext
    normDeclType <- whnf declCtx declType
    traverseAuxFreeVarArgs f p normDeclType declArgs

  (_, _) ->
    compilerDeveloperError $
      "Malformed type and arguments found when traversing auxilarity arguments" <> line <>
      indent 2 ("Type:" <+> prettyVerbose declType <> line <>
                "Args:" <+> prettyVerbose declArgs)

-------------------------------------------------------------------------------
-- Inserting polarity and linearity annotations

-- | Inserts holes for all the non-user facing auxilliary annotations, e.g.
-- polarity annotations on the `Bool` type and linearity annotations on the
-- `Rat` type.
insertHolesForAuxiliaryAnnotations :: (TCM m
                                     , TraverseAuxiliaryArguments a)
                                   => a
                                   -> m a
insertHolesForAuxiliaryAnnotations e =
  logCompilerPass MaxDetail "insertion of missing auxiliary types" $
    traverseAux insertionUpdateFn e

insertionUpdateFn :: TCM m => AuxArgUpdate m
insertionUpdateFn p auxType = \case
  Just e -> return e
  Nothing -> case auxType of
    Lin -> freshLinearityMeta p
    Pol -> freshPolarityMeta p

-------------------------------------------------------------------------------
-- Inserting polarity and linearity constraints to capture function application

-- | Function for inserting function input and output constraints. Traverses
-- the declaration type, replacing linearity and polarity types with fresh
-- meta variables, and then relates the the two by adding a new suitable
-- constraint.
addFunctionAuxiliaryInputOutputConstraints :: TCM m
                                           => CheckedDecl
                                           -> m CheckedDecl
addFunctionAuxiliaryInputOutputConstraints = \case
  DefFunction p ident t e -> do
    logCompilerPass MaxDetail "insertion of function constraints" $ do
      DefFunction p ident <$> decomposePiType (ident, p) 0 t <*> pure e

  d -> return d

decomposePiType :: TCM m => DeclProvenance -> Int -> CheckedType -> m CheckedType
decomposePiType declProv@(ident, p) explicitBindingDepth = \case
  Pi p' binder res
    | isExplicit binder -> do
      let position = FunctionInput (nameOf ident) explicitBindingDepth
      newBinder <- traverseAux (replaceAux (p, position)) binder
      Pi p' newBinder <$> decomposePiType declProv (explicitBindingDepth + 1) res
    | otherwise -> do
      Pi p' binder <$> decomposePiType declProv explicitBindingDepth res

  outputType -> do
    let position = FunctionOutput (nameOf ident)
    traverseAux (replaceAux (p, position)) outputType

replaceAux :: TCM m => (Provenance, FunctionPosition) -> AuxArgUpdate m
replaceAux position p auxType = \case
  Nothing   -> compilerDeveloperError
    "Should not be missing auxiliary arguments during function constraint insertion"
  Just expr -> case auxType of
    Lin -> do
      newLin <- freshLinearityMeta p
      addFunctionConstraint (LinearityTypeClass . FunctionLinearity) position expr newLin
      return newLin
    Pol -> do
      newPol <- freshPolarityMeta p
      addFunctionConstraint (PolarityTypeClass . FunctionPolarity) position expr newPol
      return newPol

addFunctionConstraint :: TCM m
                      => (FunctionPosition -> TypeClass)
                      -> (Provenance, FunctionPosition)
                      -> CheckedExpr
                      -> CheckedExpr
                      -> m ()
addFunctionConstraint mkTC (declProv, position) existingExpr newMeta = do
  let constraintArgs = ExplicitArg (provenanceOf existingExpr) <$> case position of
          FunctionInput{}  -> [newMeta, existingExpr]
          FunctionOutput{} -> [existingExpr, newMeta]
  let tc = mkTC position
  let constraint = BuiltinTypeClass declProv tc constraintArgs

  m <- freshTypeClassPlacementMeta declProv constraint
  addTypeClassConstraint declProv mempty m constraint

  return ()
