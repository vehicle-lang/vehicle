module Vehicle.Compile.Type
  ( typeCheckModuleDecls,
  )
where

import Control.Monad (forM, when)
import Control.Monad.Except (MonadError (..))
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Compile.Dependency (completelyUnusedDeclarations)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.Constraint.ApplicationSolver (runApplicationSolver)
import Vehicle.Compile.Type.Constraint.InstanceDefaultSolver (addNewInstanceConstraintUsingDefaults)
import Vehicle.Compile.Type.Constraint.InstanceSolver (runInstanceSolver)
import Vehicle.Compile.Type.Constraint.UnificationSolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Generalise
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.System (HasTypeSystem (..), TCM, lookupBuiltin, runAuxiliarySolver)
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin)
import Vehicle.Data.Builtin.Interface.Type (TypableBuiltin (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ModuleInterface
import Vehicle.Data.Code.Value (FreeEnv)
import Vehicle.Data.Variable.Free.Context (addDeclToContext)

-------------------------------------------------------------------------------
-- Interface

typeCheckModuleDecls ::
  (MonadCompile m, HasTypeSystem builtin) =>
  ModulePath ->
  ImportedModuleContext builtin ->
  [Decl Builtin] ->
  m ([Decl builtin], ModuleTypingInterface builtin, FreeEnv builtin)
typeCheckModuleDecls modulePath importedCtx decls = do
  logCompilerPass TypeChecking $ do
    runTypeCheckerTInitially importedCtx $ do
      let unusedDecls
            | modulePath == userModulePath = completelyUnusedDeclarations decls
            | otherwise = mempty
      typeCheckDecls unusedDecls decls

typeCheckDecls :: (TCM builtin m) => Set Identifier -> [Decl Builtin] -> m [Decl builtin]
typeCheckDecls unusedDecls = \case
  [] -> return []
  d : ds -> do
    typedDecl <- typeCheckDecl d (identifierOf d `Set.member` unusedDecls)
    checkedDecls <- addDeclToContext typedDecl $ typeCheckDecls unusedDecls ds
    return $ typedDecl : checkedDecls

-------------------------------------------------------------------------------
-- Type-class for things that can be type-checked

typeCheckDecl :: forall builtin m. (TCM builtin m) => Decl Builtin -> DeclIsUnused -> m (Decl builtin)
typeCheckDecl uncheckedDecl isUnused =
  logCompilerSection2 MidDetail ("typing" <+> quotePretty (identifierOf uncheckedDecl)) $ do
    logDebug MidDetail $ prettyExternal uncheckedDecl <> line

    convertedDecl <- logCompilerSection MaxDetail "Converting builtins" $ do
      traverse convertFromStandardBuiltins uncheckedDecl

    logDebug MidDetail $ prettyExternal convertedDecl

    setCurrentDecl $ Just (convertedDecl, isUnused)

    decl <- case convertedDecl of
      DefAbstract p n r t -> typeCheckAbstractDef p n r t isUnused
      DefFunction p n b t e -> typeCheckFunctionDef p n b t e isUnused
      DefRecord p n b ts fs -> typeCheckRecordDef p n b ts fs isUnused
    checkAllUnknownsSolved (Proxy @builtin)
    finalDecl <- substMetaVariables decl
    logCompilerPassOutput $ prettyExternal finalDecl
    setCurrentDecl @builtin Nothing

    return finalDecl

typeCheckAbstractDef ::
  forall builtin m.
  (TCM builtin m) =>
  Provenance ->
  Identifier ->
  DefAbstractSort ->
  Type builtin ->
  DeclIsUnused ->
  m (Decl builtin)
typeCheckAbstractDef p ident defSort uncheckedType isUnused = do
  checkedType <- checkDeclType ident uncheckedType
  finalCheckedType <- restrictAbstractDefType defSort (ident, p) checkedType
  setCurrentDecl $ Just (DefAbstract p ident defSort finalCheckedType, isUnused)

  solveConstraints (Proxy @builtin)
  let substDecl = DefAbstract p ident defSort finalCheckedType

  logUnsolvedUnknowns (Proxy @builtin)

  if defSort == BuiltinDef
    then do
      addBuiltinTypeToDatabase (lookupBuiltin (nameOf ident)) checkedType
      return substDecl
    else do
      generaliseOverUnsolvedMetasAndConstraints substDecl

typeCheckFunctionDef ::
  forall builtin m.
  (TCM builtin m) =>
  Provenance ->
  Identifier ->
  [Annotation] ->
  Type builtin ->
  Expr builtin ->
  DeclIsUnused ->
  m (Decl builtin)
typeCheckFunctionDef p ident anns typ body isUnused = do
  checkedType <- checkDeclType ident typ
  finalCheckedType <-
    if isAnnotatedAsProperty anns
      then logCompilerSection2 MidDetail "checking suitability of type as @property" $ do
        restrictDeclType RestrictedProperty (ident, p) checkedType
      else return checkedType

  -- Type check the body.
  let pass = bidirectionalPassDoc <+> "body of" <+> quotePretty ident
  checkedBody <-
    logCompilerSection2 MidDetail pass $
      checkExprType mempty Relevant finalCheckedType body

  -- Reconstruct the function.
  let checkedDecl = DefFunction p ident anns finalCheckedType checkedBody

  -- Solve constraints and substitute through.
  setCurrentDecl $ Just (checkedDecl, isUnused)
  solveConstraints (Proxy @builtin)
  substDecl <- substMetaVariables checkedDecl

  if isAnnotatedAsProperty anns
    then return substDecl
    else do
      -- Otherwise if not a property then generalise over unsolved meta-variables.
      checkedDecl1 <-
        if isUserCode ident
          then addAuxiliaryInputOutputConstraints substDecl
          else return substDecl

      logUnsolvedUnknowns (Proxy @builtin)

      generaliseOverUnsolvedMetasAndConstraints checkedDecl1

typeCheckRecordDef ::
  forall builtin m.
  (TCM builtin m) =>
  Provenance ->
  Identifier ->
  [Annotation] ->
  Telescope builtin ->
  RecordFields (Type builtin) ->
  DeclIsUnused ->
  m (Decl builtin)
typeCheckRecordDef p ident anns uncheckedTelescope uncheckedFields isUnused = do
  (checkedTelescope, checkedFields) <-
    checkRecordTelescopeAndFields ident uncheckedTelescope uncheckedFields

  -- Reconstruct the function.
  let checkedDecl = DefRecord p ident anns checkedTelescope checkedFields

  -- Solve constraints and substitute through.
  setCurrentDecl $ Just (checkedDecl, isUnused)
  solveConstraints (Proxy @builtin)

  -- Check if the record is annotated as a tensor and check the restrictions.
  -- Needs to be done after the main solving pass so that we are guaranteed to
  -- get the most informative error message.
  when (isAnnotatedAsTensor anns) $
    logCompilerSection2 MidDetail "checking suitability of type as @tensor" $ do
      checkedFields' <- traverseRecordFields (substMetaVariablesAt mempty) checkedFields
      restrictRecordAnnotatedAsTensor (ident, p) checkedFields'
      solveConstraints (Proxy @builtin)

  substMetaVariables checkedDecl

checkRecordTelescopeAndFields ::
  forall builtin m.
  (TCM builtin m) =>
  Identifier ->
  Telescope builtin ->
  RecordFields (Type builtin) ->
  m (Telescope builtin, RecordFields (Type builtin))
checkRecordTelescopeAndFields ident telescope fields =
  runMonadBidirectional @m @builtin mempty Relevant $ do
    checkTelescope telescope $ do
      -- Type check the body.
      let pass = bidirectionalPassDoc <+> "fields of" <+> quotePretty ident
      checkedFields <-
        logCompilerSection2 MidDetail pass $
          traverse (checkRecordFieldDef ident) fields
      return checkedFields

checkRecordFieldDef ::
  (TCM builtin m) =>
  Identifier ->
  RecordField (Type builtin) ->
  m (RecordField (Type builtin))
checkRecordFieldDef _recordIdent (field, fieldType) = do
  checkedFieldType <- checkDeclType field fieldType
  return (field, checkedFieldType)

checkDeclType :: (TCM builtin m, HasName name Name) => name -> Type builtin -> m (Type builtin)
checkDeclType ident declType = do
  let pass = bidirectionalPassDoc <+> "type of" <+> quotePretty (nameOf ident)
  logCompilerSection2 MidDetail pass $ do
    checkExprType mempty Relevant (TypeUniverse mempty 0) declType

restrictAbstractDefType ::
  (TCM builtin m) =>
  DefAbstractSort ->
  DeclProvenance ->
  Type builtin ->
  m (Type builtin)
restrictAbstractDefType resource decl@(ident, _) defType = do
  let resourceName = pretty resource <+> quotePretty ident
  logCompilerSection2 MidDetail ("checking suitability of the type of" <+> resourceName) $ do
    case resource of
      ParameterDef sort -> restrictDeclType (RestrictedParameter sort) decl defType
      DatasetDef -> restrictDeclType RestrictedDataset decl defType
      NetworkDef -> restrictDeclType RestrictedNetwork decl defType
      BuiltinDef {} -> return defType

-------------------------------------------------------------------------------
-- Constraint solving

-- | Tries to solve constraints. Passes in the type of the current declaration
-- being checked, as metas are handled different according to whether they
-- occur in the type or not.
solveConstraints :: forall builtin m. (TCM builtin m) => Proxy builtin -> m ()
solveConstraints proxy = logCompilerSection2 MidDetail "constraint solving" $ do
  sortConstraints
  loopOverConstraints 1
  where
    sortConstraints :: m ()
    sortConstraints = do
      -- We try and priortise resolving the cast constraints first (e.g. HasTensor, IsNatLiteral)
      -- as it produces far better error messages for the user.
      instanceConstraints <- getActiveInstanceConstraints @builtin
      let sortedInstanceConstraints = sortOn (not . isCastConstraint . goalHead . instanceGoal . objectIn) instanceConstraints
      setInstanceConstraints sortedInstanceConstraints

    loopOverConstraints :: (TCM builtin m) => Int -> m ()
    loopOverConstraints loopNumber = do
      logUnsolvedUnknowns proxy

      -- Try to solve the constraints pass
      oldConstraintIDS <- getActiveConstraintIDs proxy
      logCompilerSection2 MaxDetail ("constraint solving pass" <+> pretty loopNumber) runSolvers
      newConstraintIDS <- getActiveConstraintIDs proxy

      if IntSet.null newConstraintIDS
        then return ()
        else
          if newConstraintIDS /= oldConstraintIDS
            then loopOverConstraints (loopNumber + 1)
            else do
              -- If no constraints are unblocked then try generating new constraints using defaults.
              logDebug MaxDetail $ "Temporarily stuck" <> line
              success <- tryToUnstick
              when success $
                -- If new constraints generated then continue solving.
                loopOverConstraints (loopNumber + 1)

    runSolvers :: (TCM builtin m) => m ()
    runSolvers = do
      runApplicationSolver proxy
      runUnificationSolver proxy True
      runInstanceSolver proxy 0
      runAuxiliarySolver proxy

    tryToUnstick :: (TCM builtin m) => m Bool
    tryToUnstick = do
      -- First try to increase the depth limit for instance search
      solvedMetas <- logCompilerSection2 MidDetail "trying to increase the depth for instance search" $ do
        trackSolvedMetas proxy $ runInstanceSolver proxy 1

      if not (MetaSet.null solvedMetas)
        then return True
        else do
          -- Then if that fails try to use default instances
          success <- logCompilerSection2 MidDetail "trying to generate a new constraint using instance defaults" $ do
            addNewInstanceConstraintUsingDefaults proxy

          if success
            then return True
            else logCompilerSection2 MidDetail "trying to generate a new constraint using instance defaults" $ do
              -- Then if that fails try to use default auxiliary instances
              generateDefaultAuxiliaryConstraint proxy

-------------------------------------------------------------------------------
-- Unsolved constraint checks

checkAllUnknownsSolved :: forall builtin m. (MonadTypeChecker builtin m, NormalisableBuiltin builtin) => Proxy builtin -> m ()
checkAllUnknownsSolved proxy = do
  -- First check all user constraints (i.e. unification and type-class
  -- constraints) are solved.
  checkAllConstraintsSolved proxy getActiveConstraints id
  -- Then check all meta-variables have been solved.
  checkAllMetasSolved proxy
  -- Then clear the meta-ctx
  clearMetaCtx proxy
  -- ...and the fresh names
  clearFreshNames proxy

checkAllMetasSolved :: forall builtin m. (MonadTypeChecker builtin m, Eq builtin, NormalisableBuiltin builtin) => Proxy builtin -> m ()
checkAllMetasSolved proxy = do
  unsolvedMetas <- getUnsolvedMetas proxy
  case MetaSet.toList unsolvedMetas of
    [] -> return ()
    m : ms -> do
      metasAndOrigins <-
        forM
          (m :| ms)
          ( \meta -> do
              origin <- getMetaProvenance proxy meta
              return (meta, origin)
          )
      logUnsolvedUnknowns proxy
      throwError $ TypingError $ UnsolvedMetas proxy metasAndOrigins

bidirectionalPassDoc :: Doc a
bidirectionalPassDoc = "bidirectional pass over"
