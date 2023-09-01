module Vehicle.Compile.Type
  ( typeCheckProg,
    typeCheckExpr,
    runUnificationSolver,
  )
where

import Control.Monad (forM, unless, when)
import Control.Monad.Except (MonadError (..))
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Context.Free.Class
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.Constraint.Core (runConstraintSolver)
import Vehicle.Compile.Type.Constraint.UnificationSolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Generalise
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalised

-------------------------------------------------------------------------------
-- Algorithm

typeCheckProg ::
  forall builtin m.
  (HasTypeSystem builtin, MonadCompile m) =>
  InstanceCandidateDatabase builtin ->
  FreeCtx builtin ->
  Prog Ix Builtin ->
  m (Prog Ix builtin)
typeCheckProg instanceCandidates freeCtx (Main uncheckedProg) =
  logCompilerPass MinDetail "type checking" $
    runTypeCheckerTInitially freeCtx instanceCandidates $ do
      xs <- typeCheckDecls uncheckedProg
      return $ Main xs

typeCheckExpr ::
  forall builtin m.
  (HasTypeSystem builtin, MonadCompile m) =>
  InstanceCandidateDatabase builtin ->
  FreeCtx builtin ->
  Expr Ix Builtin ->
  m (Expr Ix builtin)
typeCheckExpr instanceCandidates freeCtx expr1 = do
  runTypeCheckerTInitially freeCtx instanceCandidates $ do
    expr2 <- convertExprFromStandardTypes expr1
    (expr3, _exprType) <- runMonadBidirectional (Proxy @builtin) $ inferExpr expr2
    solveConstraints @builtin Nothing
    expr4 <- substMetas expr3
    checkAllUnknownsSolved (Proxy @builtin)
    return expr4

-------------------------------------------------------------------------------
-- Type-class for things that can be type-checked

typeCheckDecls :: (TCM builtin m) => [Decl Ix Builtin] -> m [Decl Ix builtin]
typeCheckDecls = \case
  [] -> return []
  d : ds -> do
    typedDecl <- typeCheckDecl d
    checkedDecls <- addDeclToContext typedDecl $ typeCheckDecls ds
    return $ typedDecl : checkedDecls

typeCheckDecl :: forall builtin m. (TCM builtin m) => Decl Ix Builtin -> m (Decl Ix builtin)
typeCheckDecl uncheckedDecl =
  logCompilerPass MaxDetail ("declaration" <+> quotePretty (identifierOf uncheckedDecl)) $ do
    convertedDecl <- traverse convertExprFromStandardTypes uncheckedDecl

    decl <- case convertedDecl of
      DefAbstract p n r t -> typeCheckAbstractDef p n r t
      DefFunction p n b t e -> typeCheckFunction p n b t e

    checkAllUnknownsSolved (Proxy @builtin)
    finalDecl <- substMetas decl
    logCompilerPassOutput $ prettyFriendly finalDecl

    return finalDecl

convertExprFromStandardTypes ::
  forall builtin m.
  (HasTypeSystem builtin, TCM builtin m) =>
  Expr Ix Builtin ->
  m (Expr Ix builtin)
convertExprFromStandardTypes = traverseBuiltinsM convertFromStandardBuiltins

typeCheckAbstractDef ::
  (TCM builtin m) =>
  Provenance ->
  Identifier ->
  DefAbstractSort ->
  Type Ix builtin ->
  m (Decl Ix builtin)
typeCheckAbstractDef p ident defSort uncheckedType = do
  checkedType <- checkDeclType ident uncheckedType
  let checkedDecl = DefAbstract p ident defSort checkedType
  solveConstraints (Just checkedDecl)
  substCheckedType <- substMetas checkedType

  -- Add extra constraints on the type. Need to have called
  -- solve constraints beforehand in order to allow for normalisation,
  -- but really only need to have solved type-class constraints.
  logDebug MaxDetail $ prettyVerbose substCheckedType
  gluedType <- glueNBE mempty substCheckedType
  updatedCheckedType <- restrictAbstractDefType defSort (ident, p) gluedType
  let updatedCheckedDecl = DefAbstract p ident defSort updatedCheckedType

  solveConstraints (Just updatedCheckedDecl)

  substDecl <- substMetas updatedCheckedDecl
  logUnsolvedUnknowns (Just substDecl) Nothing

  finalDecl <- generaliseOverUnsolvedMetaVariables substDecl
  return finalDecl

typeCheckFunction ::
  forall builtin m.
  (TCM builtin m) =>
  Provenance ->
  Identifier ->
  [Annotation] ->
  Type Ix builtin ->
  Expr Ix builtin ->
  m (Decl Ix builtin)
typeCheckFunction p ident anns typ body = do
  checkedType <- checkDeclType ident typ

  -- Type check the body.
  let pass = bidirectionalPassDoc <+> "body of" <+> quotePretty ident
  checkedBody <- logCompilerPass MidDetail pass $ do
    runMonadBidirectional (Proxy @builtin) $ checkExpr checkedType body

  -- Reconstruct the function.
  let checkedDecl = DefFunction p ident anns checkedType checkedBody

  -- Solve constraints and substitute through.
  solveConstraints (Just checkedDecl)
  substDecl <- substMetas checkedDecl

  if isProperty anns
    then do
      gluedDeclType <- glueNBE mempty (typeOf substDecl)
      restrictPropertyType (ident, p) gluedDeclType
      solveConstraints (Just substDecl)
      substAgainDecl <- substMetas substDecl
      return substAgainDecl
    else do
      -- Otherwise if not a property then generalise over unsolved meta-variables.
      checkedDecl1 <-
        if moduleOf ident == User
          then addAuxiliaryInputOutputConstraints substDecl
          else return substDecl
      logUnsolvedUnknowns (Just substDecl) Nothing

      checkedDecl2 <- generaliseOverUnsolvedConstraints checkedDecl1
      checkedDecl3 <- generaliseOverUnsolvedMetaVariables checkedDecl2
      return checkedDecl3

checkDeclType :: forall builtin m. (TCM builtin m) => Identifier -> Expr Ix builtin -> m (Type Ix builtin)
checkDeclType ident declType = do
  let pass = bidirectionalPassDoc <+> "type of" <+> quotePretty ident
  logCompilerPass MidDetail pass $ do
    runMonadBidirectional (Proxy @builtin) $ checkExpr (TypeUniverse mempty 0) declType

restrictAbstractDefType ::
  (TCM builtin m) =>
  DefAbstractSort ->
  DeclProvenance ->
  GluedType builtin ->
  m (Type Ix builtin)
restrictAbstractDefType resource decl@(ident, _) defType = do
  let resourceName = pretty resource <+> quotePretty ident
  logCompilerPass MidDetail ("checking suitability of the type of" <+> resourceName) $ do
    case resource of
      ParameterDef sort -> restrictParameterType sort decl defType
      DatasetDef -> restrictDatasetType decl defType
      NetworkDef -> restrictNetworkType decl defType
      PostulateDef -> return $ unnormalised defType

-------------------------------------------------------------------------------
-- Constraint solving

-- | Tries to solve constraints. Passes in the type of the current declaration
-- being checked, as metas are handled different according to whether they
-- occur in the type or not.
solveConstraints :: forall builtin m. (TCM builtin m) => Maybe (Decl Ix builtin) -> m ()
solveConstraints d = logCompilerPass MidDetail "constraint solving" $ do
  loopOverConstraints mempty 1 d
  where
    loopOverConstraints :: (TCM builtin m) => MetaSet -> Int -> Maybe (Decl Ix builtin) -> m ()
    loopOverConstraints recentlySolvedMetas loopNumber decl = do
      unsolvedConstraints <- getActiveConstraints @builtin

      updatedDecl <- traverse substMetas decl
      logUnsolvedUnknowns updatedDecl (Just recentlySolvedMetas)

      unless (null unsolvedConstraints) $ do
        let allConstraintsBlocked = all (constraintIsBlocked recentlySolvedMetas) unsolvedConstraints

        if allConstraintsBlocked
          then do
            -- If no constraints are unblocked then try generating new constraints using defaults.
            successfullyGeneratedDefault <- generateDefaultConstraint decl
            when successfullyGeneratedDefault $
              -- If new constraints generated then continue solving.
              loopOverConstraints mempty loopNumber decl
          else do
            -- If we have made useful progress then start a new pass
            let passDoc = "constraint solving pass" <+> pretty loopNumber
            newMetasSolved <- logCompilerPass MaxDetail passDoc $ do
              metasSolvedDuringUnification <-
                trackSolvedMetas (Proxy @builtin) $
                  runUnificationSolver (Proxy @builtin) recentlySolvedMetas

              logUnsolvedUnknowns updatedDecl (Just recentlySolvedMetas)

              metasSolvedDuringInstanceResolution <-
                trackSolvedMetas (Proxy @builtin) $
                  runInstanceSolver (Proxy @builtin) metasSolvedDuringUnification
              return metasSolvedDuringInstanceResolution

            loopOverConstraints newMetasSolved (loopNumber + 1) updatedDecl

-- | Attempts to solve as many type-class constraints as possible. Takes in
-- the set of meta-variables solved since the solver was last run and outputs
-- the set of meta-variables solved during this run.
runInstanceSolver :: forall builtin m. (TCM builtin m) => Proxy builtin -> MetaSet -> m ()
runInstanceSolver _ metasSolved = do
  instanceCandidates <- getInstanceCandidates
  logCompilerPass MaxDetail ("instance solver run" <> line) $
    runConstraintSolver @builtin
      getActiveInstanceConstraints
      setInstanceConstraints
      (solveInstance instanceCandidates)
      metasSolved

-- | Attempts to solve as many unification constraints as possible. Takes in
-- the set of meta-variables solved since unification was last run and outputs
-- the set of meta-variables solved during this run.
runUnificationSolver :: forall builtin m. (TCM builtin m) => Proxy builtin -> MetaSet -> m ()
runUnificationSolver _ metasSolved =
  logCompilerPass MaxDetail "unification solver run" $
    runConstraintSolver @builtin
      getActiveUnificationConstraints
      setUnificationConstraints
      solveUnificationConstraint
      metasSolved

-------------------------------------------------------------------------------
-- Unsolved constraint checks

checkAllUnknownsSolved :: (TCM builtin m) => Proxy builtin -> m ()
checkAllUnknownsSolved proxy = do
  -- First check all user constraints (i.e. unification and type-class
  -- constraints) are solved.
  checkAllConstraintsSolved proxy
  -- Then check all meta-variables have been solved.
  checkAllMetasSolved proxy
  -- Then clear the meta-ctx
  clearMetaCtx proxy
  -- ...and the fresh names
  clearFreshNames proxy

checkAllConstraintsSolved :: forall builtin m. (TCM builtin m) => Proxy builtin -> m ()
checkAllConstraintsSolved _ = do
  constraints <- getActiveConstraints @builtin
  case constraints of
    [] -> return ()
    (c : cs) -> throwError $ TypingError $ UnsolvedConstraints (c :| cs)

checkAllMetasSolved :: (TCM builtin m) => Proxy builtin -> m ()
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
      throwError $ UnsolvedMetas metasAndOrigins

logUnsolvedUnknowns :: forall builtin m. (TCM builtin m) => Maybe (Decl Ix builtin) -> Maybe MetaSet -> m ()
logUnsolvedUnknowns maybeDecl maybeSolvedMetas = do
  logDebugM MaxDetail $ do
    newSubstitution <- getMetaSubstitution (Proxy @builtin)
    updatedSubst <- substMetas newSubstitution

    unsolvedMetas <- getUnsolvedMetas (Proxy @builtin)
    unsolvedMetasDoc <- prettyMetas (Proxy @builtin) unsolvedMetas
    unsolvedConstraints <- getActiveConstraints @builtin

    let constraintsDoc = case maybeSolvedMetas of
          Nothing ->
            "unsolved-constraints:"
              <> line
              <> indent 2 (prettyVerbose unsolvedConstraints)
              <> line
          Just solvedMetas -> do
            let isUnblocked = not . constraintIsBlocked solvedMetas
            let (unblockedConstraints, blockedConstraints) = partition isUnblocked unsolvedConstraints
            "unsolved-blocked-constraints:"
              <> line
              <> indent 2 (prettyBlockedConstraints blockedConstraints)
              <> line
              <> "unsolved-unblocked-constraints:"
              <> line
              <> indent 2 (prettyVerbose unblockedConstraints)
              <> line

    let declDoc = case maybeDecl of
          Nothing -> ""
          Just decl ->
            "current-decl:"
              <> line
              <> indent 2 (prettyVerbose decl)
              <> line

    return $
      "current-solution:"
        <> line
        <> prettyVerbose (fmap unnormalised updatedSubst)
        <> line
        <> "unsolved-metas:"
        <> line
        <> indent 2 unsolvedMetasDoc
        <> line
        <> constraintsDoc
        <> declDoc

prettyBlockedConstraints :: (PrintableBuiltin builtin) => [WithContext (Constraint builtin)] -> Doc a
prettyBlockedConstraints constraints = do
  let pairs = fmap (\c -> prettyFriendly c <> "   " <> pretty (blockedBy $ contextOf c)) constraints
  prettySetLike pairs

bidirectionalPassDoc :: Doc a
bidirectionalPassDoc = "bidirectional pass over"
