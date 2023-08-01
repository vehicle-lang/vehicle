module Vehicle.Compile.Type
  ( typeCheckProg,
    typeCheckExpr,
    runUnificationSolver,
  )
where

import Control.Monad (forM, unless, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..))
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map (fromList)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Monad (getMetaSubstitution)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.Constraint.Core (runConstraintSolver)
import Vehicle.Compile.Type.Constraint.UnificationSolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Generalise
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Substitution
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

-------------------------------------------------------------------------------
-- Algorithm

typeCheckProg ::
  (TypableBuiltin builtin, MonadCompile m) =>
  Imports builtin ->
  InstanceCandidateDatabase builtin ->
  Prog Ix StandardBuiltin ->
  m (GluedProg builtin)
typeCheckProg imports instanceCandidates (Main uncheckedProg) =
  logCompilerPass MinDetail "type checking" $
    runTypeChecker (createDeclCtx imports) instanceCandidates $ do
      Main <$> typeCheckDecls uncheckedProg

typeCheckExpr ::
  forall builtin m.
  (TypableBuiltin builtin, MonadCompile m) =>
  Imports builtin ->
  InstanceCandidateDatabase builtin ->
  Expr Ix builtin ->
  m (Expr Ix builtin)
typeCheckExpr imports instanceCandidates expr1 =
  runTypeChecker (createDeclCtx imports) instanceCandidates $ do
    (expr3, _exprType) <- runReaderT (inferExpr expr1) mempty
    solveConstraints @builtin Nothing
    expr4 <- substMetas expr3
    -- expr5 <- removeIrrelevantCode expr4
    checkAllUnknownsSolved (Proxy @builtin)
    return expr4

-------------------------------------------------------------------------------
-- Type-class for things that can be type-checked

typeCheckDecls :: (TCM builtin m) => [Decl Ix StandardBuiltin] -> m [GluedDecl builtin]
typeCheckDecls = \case
  [] -> return []
  d : ds -> do
    typedDecl <- typeCheckDecl d
    checkedDecls <- addDeclContext typedDecl $ typeCheckDecls ds
    return $ typedDecl : checkedDecls

typeCheckDecl :: forall builtin m. (TCM builtin m) => Decl Ix StandardBuiltin -> m (GluedDecl builtin)
typeCheckDecl uncheckedDecl =
  logCompilerPass MaxDetail ("declaration" <+> quotePretty (identifierOf uncheckedDecl)) $ do
    convertedDecl <- traverse convertExprFromStandardTypes uncheckedDecl

    gluedDecl <- case convertedDecl of
      DefAbstract p n r t -> typeCheckAbstractDef p n r t
      DefFunction p n b t e -> typeCheckFunction p n b t e

    checkAllUnknownsSolved (Proxy @builtin)
    finalDecl <- substMetas gluedDecl
    logCompilerPassOutput $ prettyFriendly (fmap unnormalised finalDecl)

    return finalDecl

convertExprFromStandardTypes ::
  forall builtin m.
  (TypableBuiltin builtin, TCM builtin m) =>
  Expr Ix StandardBuiltin ->
  m (Expr Ix builtin)
convertExprFromStandardTypes = traverseBuiltinsM convertFromStandardTypes

typeCheckAbstractDef ::
  (TCM builtin m) =>
  Provenance ->
  Identifier ->
  DefAbstractSort ->
  Type Ix builtin ->
  m (GluedDecl builtin)
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
  gluedDecl <- traverse (glueNBE mempty) finalDecl

  return gluedDecl

typeCheckFunction ::
  (TCM builtin m) =>
  Provenance ->
  Identifier ->
  [Annotation] ->
  Type Ix builtin ->
  Expr Ix builtin ->
  m (GluedDecl builtin)
typeCheckFunction p ident anns typ body = do
  checkedType <- checkDeclType ident typ

  -- Type check the body.
  let pass = bidirectionalPassDoc <+> "body of" <+> quotePretty ident
  checkedBody <- logCompilerPass MidDetail pass $ do
    runReaderT (checkExpr checkedType body) mempty

  -- Reconstruct the function.
  let checkedDecl = DefFunction p ident anns checkedType checkedBody

  -- Solve constraints and substitute through.
  solveConstraints (Just checkedDecl)
  substDecl <- substMetas checkedDecl

  if isProperty anns
    then do
      gluedDecl <- traverse (glueNBE mempty) substDecl
      restrictPropertyType (ident, p) (typeOf gluedDecl)
      return gluedDecl
    else do
      -- Otherwise if not a property then generalise over unsolved meta-variables.
      checkedDecl1 <-
        if moduleOf ident == User
          then addAuxiliaryInputOutputConstraints substDecl
          else return substDecl
      logUnsolvedUnknowns (Just substDecl) Nothing

      checkedDecl2 <- generaliseOverUnsolvedConstraints checkedDecl1
      checkedDecl3 <- generaliseOverUnsolvedMetaVariables checkedDecl2
      gluedDecl <- traverse (glueNBE mempty) checkedDecl3
      return gluedDecl

checkDeclType :: (TCM builtin m) => Identifier -> Expr Ix builtin -> m (Type Ix builtin)
checkDeclType ident declType = do
  let pass = bidirectionalPassDoc <+> "type of" <+> quotePretty ident
  logCompilerPass MidDetail pass $ do
    (checkedType, typeOfType) <- runReaderT (inferExpr declType) mempty
    assertDeclTypeIsType ident typeOfType
    return checkedType

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

assertDeclTypeIsType :: (TCM builtin m) => Identifier -> Type Ix builtin -> m ()
-- This is a bit of a hack to get around having to have a solver for universe
-- levels. As type definitions will always have an annotated Type 0 inserted
-- by delaboration, we can match on it here. Anything else will be unified
-- with type 0.
assertDeclTypeIsType _ TypeUniverse {} = return ()
assertDeclTypeIsType ident actualType = do
  let p = provenanceOf actualType
  let expectedType = TypeUniverse p 0
  let origin = CheckingExprType (FreeVar p ident) expectedType actualType
  createFreshUnificationConstraint p mempty origin expectedType actualType
  return ()

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
    (c : cs) -> handleTypingError $ UnsolvableConstraints (c :| cs)

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
    newSubstitution <- getMetaSubstitution @builtin
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

-------------------------------------------------------------------------------
-- Other

createDeclCtx :: Imports builtin -> TypingDeclCtx builtin
createDeclCtx imports =
  Map.fromList $
    [getEntry d | imp <- imports, let Main ds = imp, d <- ds]
  where
    getEntry :: GluedDecl builtin -> (Identifier, TypingDeclCtxEntry builtin)
    getEntry d = do
      let ident = identifierOf d
      (ident, mkTypingDeclCtxEntry d)
