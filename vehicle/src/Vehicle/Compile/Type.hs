module Vehicle.Compile.Type
  ( typeCheckProg,
    typeCheckSolitaryExpr,
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
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Dependency (completelyUnusedDeclarations, createDependencyGraph)
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
import Vehicle.Compile.Type.System (HasTypeSystem (..), TCM, runAuxiliarySolver)
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin)
import Vehicle.Data.Builtin.Interface.Type (TypableBuiltin (..))
import Vehicle.Data.Builtin.Standard

-------------------------------------------------------------------------------
-- Algorithm

typeCheckProg ::
  forall builtin m.
  (HasTypeSystem builtin, NormalisableBuiltin builtin, MonadCompile m) =>
  Module ->
  InstanceDatabase builtin ->
  FreeCtx builtin ->
  Prog Builtin ->
  m (Prog builtin)
typeCheckProg modul instanceCandidates freeCtx prog@(Main uncheckedProg) =
  logCompilerPass MinDetail "type checking" $
    runTypeCheckerTInitially freeCtx instanceCandidates $ do
      let unusedDecls = case modul of
            User -> completelyUnusedDeclarations (createDependencyGraph prog)
            _ -> mempty
      logDebug MaxDetail $ "Good" <+> prettySet unusedDecls
      xs <- typeCheckDecls unusedDecls uncheckedProg
      return $ Main xs

typeCheckSolitaryExpr ::
  forall builtin m.
  (HasTypeSystem builtin, NormalisableBuiltin builtin, MonadCompile m) =>
  InstanceDatabase builtin ->
  FreeCtx builtin ->
  Expr Builtin ->
  m (Expr builtin)
typeCheckSolitaryExpr instanceCandidates freeCtx expr1 = do
  runTypeCheckerTInitially freeCtx instanceCandidates $ do
    expr2 <- convertFromStandardBuiltins expr1
    (expr3, _exprType) <- inferExprType mempty Relevant expr2
    solveConstraints (Proxy @builtin)
    expr4 <- substMetasAt 0 expr3
    checkAllUnknownsSolved (Proxy @builtin)
    return expr4

-------------------------------------------------------------------------------
-- Type-class for things that can be type-checked

typeCheckDecls :: (TCM builtin m) => Set Identifier -> [Decl Builtin] -> m [Decl builtin]
typeCheckDecls unusedDecls = \case
  [] -> return []
  d : ds -> do
    typedDecl <- typeCheckDecl d (identifierOf d `Set.member` unusedDecls)
    checkedDecls <- addDeclToContext typedDecl $ typeCheckDecls unusedDecls ds
    return $ typedDecl : checkedDecls

typeCheckDecl :: forall builtin m. (TCM builtin m) => Decl Builtin -> DeclIsUnused -> m (Decl builtin)
typeCheckDecl uncheckedDecl isUnused =
  logCompilerPass MidDetail ("typing" <+> quotePretty (identifierOf uncheckedDecl)) $ do
    logDebug MidDetail $ prettyExternal uncheckedDecl <> line

    convertedDecl <- logCompilerSection MaxDetail "Converting builtins" $ do
      traverse convertFromStandardBuiltins uncheckedDecl

    logDebug MidDetail $ prettyExternal convertedDecl

    setCurrentDecl $ Just (convertedDecl, isUnused)

    decl <- case convertedDecl of
      DefAbstract p n r t -> typeCheckAbstractDef p n r t isUnused
      DefFunction p n b t e -> typeCheckFunctionDef p n b t e isUnused
      DefRecord p n t fs -> typeCheckRecordDef p n t fs isUnused

    checkAllUnknownsSolved (Proxy @builtin)
    finalDecl <- substMetas decl
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

  finalDecl <- generaliseOverUnsolvedMetasAndConstraints substDecl
  return finalDecl

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
    if isProperty anns
      then logCompilerPass MidDetail "checking suitability of type as @property" $ do
        restrictDeclType RestrictedProperty (ident, p) checkedType
      else return checkedType

  -- Type check the body.
  let pass = bidirectionalPassDoc <+> "body of" <+> quotePretty ident
  checkedBody <-
    logCompilerPass MidDetail pass $
      checkExprType mempty Relevant finalCheckedType body

  -- Reconstruct the function.
  let checkedDecl = DefFunction p ident anns finalCheckedType checkedBody

  -- Solve constraints and substitute through.
  setCurrentDecl $ Just (checkedDecl, isUnused)
  solveConstraints (Proxy @builtin)
  substDecl <- substMetas checkedDecl

  if isProperty anns
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
  Type builtin ->
  RecordFields (Type builtin) ->
  DeclIsUnused ->
  m (Decl builtin)
typeCheckRecordDef p ident uncheckedType uncheckedFields isUnused = do
  checkedType <- checkDeclType ident uncheckedType

  -- Type check the body.
  let pass = bidirectionalPassDoc <+> "fields of" <+> quotePretty ident
  checkedFields <-
    logCompilerPass MidDetail pass $
      traverse (checkRecordFieldDef ident) uncheckedFields

  -- Reconstruct the function.
  let checkedDecl = DefRecord p ident checkedType checkedFields

  -- Solve constraints and substitute through.
  setCurrentDecl $ Just (checkedDecl, isUnused)
  solveConstraints (Proxy @builtin)
  substMetas checkedDecl

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
  logCompilerPass MidDetail pass $ do
    checkExprType mempty Relevant (TypeUniverse mempty 0) declType

restrictAbstractDefType ::
  (TCM builtin m) =>
  DefAbstractSort ->
  DeclProvenance ->
  Type builtin ->
  m (Type builtin)
restrictAbstractDefType resource decl@(ident, _) defType = do
  let resourceName = pretty resource <+> quotePretty ident
  logCompilerPass MidDetail ("checking suitability of the type of" <+> resourceName) $ do
    case resource of
      ParameterDef sort -> restrictDeclType (RestrictedParameter sort) decl defType
      DatasetDef -> restrictDeclType RestrictedDataset decl defType
      NetworkDef -> restrictDeclType RestrictedNetwork decl defType
      PostulateDef {} -> return defType

-------------------------------------------------------------------------------
-- Constraint solving

-- | Tries to solve constraints. Passes in the type of the current declaration
-- being checked, as metas are handled different according to whether they
-- occur in the type or not.
solveConstraints :: forall builtin m. (TCM builtin m) => Proxy builtin -> m ()
solveConstraints proxy = logCompilerPass MidDetail "constraint solving" $ do
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
      logCompilerPass MaxDetail ("constraint solving pass" <+> pretty loopNumber) runSolvers
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
      solvedMetas <- logCompilerPass MidDetail "trying to increase the depth for instance search" $ do
        trackSolvedMetas proxy $ runInstanceSolver proxy 1

      if not (MetaSet.null solvedMetas)
        then return True
        else do
          -- Then if that fails try to use default instances
          success <- logCompilerPass MidDetail "trying to generate a new constraint using instance defaults" $ do
            addNewInstanceConstraintUsingDefaults proxy

          if success
            then return True
            else logCompilerPass MidDetail "trying to generate a new constraint using instance defaults" $ do
              -- Then if that fails try to use default auxiliary instances
              generateDefaultAuxiliaryConstraint proxy

-------------------------------------------------------------------------------
-- Unsolved constraint checks

checkAllUnknownsSolved :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m ()
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

checkAllMetasSolved :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m ()
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
