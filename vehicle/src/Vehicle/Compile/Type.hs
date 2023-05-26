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
import Vehicle.Compile.Normalise.NBE (getMetaSubstitution)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.Constraint.Core (runConstraintSolver)
import Vehicle.Compile.Type.Constraint.UnificationSolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Generalise
import Vehicle.Compile.Type.Irrelevance
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Substitution
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised

-------------------------------------------------------------------------------
-- Algorithm

typeCheckProg ::
  (TypableBuiltin types, MonadCompile m) =>
  Imports types ->
  NormalisableProg StandardBuiltinType ->
  m (GluedProg types)
typeCheckProg imports (Main uncheckedProg) =
  logCompilerPass MinDetail "type checking" $
    runTypeChecker (createDeclCtx imports) $ do
      Main <$> typeCheckDecls uncheckedProg

typeCheckExpr ::
  forall types m.
  (TypableBuiltin types, MonadCompile m) =>
  Imports types ->
  NormalisableExpr types ->
  m (NormalisableExpr types)
typeCheckExpr imports expr1 =
  runTypeChecker (createDeclCtx imports) $ do
    (expr3, _exprType) <- runReaderT (inferExpr expr1) mempty
    solveConstraints @types Nothing
    expr4 <- substMetas expr3
    expr5 <- removeIrrelevantCode expr4
    checkAllUnknownsSolved (Proxy @types)
    return expr5

-------------------------------------------------------------------------------
-- Type-class for things that can be type-checked

typeCheckDecls :: (TCM types m) => [NormalisableDecl StandardBuiltinType] -> m [GluedDecl types]
typeCheckDecls = \case
  [] -> return []
  d : ds -> do
    typedDecl <- typeCheckDecl d
    checkedDecls <- addDeclContext typedDecl $ typeCheckDecls ds
    return $ typedDecl : checkedDecls

typeCheckDecl :: forall types m. (TCM types m) => NormalisableDecl StandardBuiltinType -> m (GluedDecl types)
typeCheckDecl uncheckedDecl =
  logCompilerPass MaxDetail ("declaration" <+> quotePretty (identifierOf uncheckedDecl)) $ do
    convertedDecl <- traverse convertExprFromStandardTypes uncheckedDecl

    gluedDecl <- case convertedDecl of
      DefAbstract p n r t -> typeCheckAbstractDef p n r t
      DefFunction p n b t e -> typeCheckFunction p n b t e

    -- when (nameOf gluedDecl == "vectorToList") $ developerError "Hi"
    checkAllUnknownsSolved (Proxy @types)
    finalDecl <- substMetas gluedDecl
    logCompilerPassOutput $ prettyFriendly (fmap unnormalised finalDecl)

    return finalDecl

convertExprFromStandardTypes ::
  forall types m.
  (TypableBuiltin types, TCM types m) =>
  NormalisableExpr StandardBuiltinType ->
  m (NormalisableExpr types)
convertExprFromStandardTypes = traverseBuiltinsM builtinUpdateFunction
  where
    builtinUpdateFunction :: BuiltinUpdate m () Ix StandardBuiltin (NormalisableBuiltin types)
    builtinUpdateFunction p1 p2 b args = do
      case b of
        CConstructor c -> return $ normAppList p1 (Builtin p2 (CConstructor c)) args
        CFunction f -> return $ normAppList p1 (Builtin p2 (CFunction f)) args
        CType t -> convertFromStandardTypes p2 t args

typeCheckAbstractDef ::
  (TCM types m) =>
  Provenance ->
  Identifier ->
  DefAbstractSort ->
  NormalisableType types ->
  m (GluedDecl types)
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
  (TCM types m) =>
  Provenance ->
  Identifier ->
  [Annotation] ->
  NormalisableType types ->
  NormalisableExpr types ->
  m (GluedDecl types)
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

checkDeclType :: (TCM types m) => Identifier -> NormalisableExpr types -> m (NormalisableType types)
checkDeclType ident declType = do
  let pass = bidirectionalPassDoc <+> "type of" <+> quotePretty ident
  logCompilerPass MidDetail pass $ do
    (checkedType, typeOfType) <- runReaderT (inferExpr declType) mempty
    assertDeclTypeIsType ident typeOfType
    return checkedType

restrictAbstractDefType ::
  (TCM types m) =>
  DefAbstractSort ->
  DeclProvenance ->
  GluedType types ->
  m (NormalisableType types)
restrictAbstractDefType resource decl@(ident, _) defType = do
  let resourceName = pretty resource <+> squotes (pretty ident)
  logCompilerPass MidDetail ("checking suitability of the type of" <+> resourceName) $ do
    case resource of
      ParameterDef sort -> restrictParameterType sort decl defType
      DatasetDef -> restrictDatasetType decl defType
      NetworkDef -> restrictNetworkType decl defType
      PostulateDef -> return $ unnormalised defType

assertDeclTypeIsType :: (TCM types m) => Identifier -> NormalisableType types -> m ()
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
solveConstraints :: forall types m. (TCM types m) => Maybe (NormalisableDecl types) -> m ()
solveConstraints d = logCompilerPass MidDetail "constraint solving" $ do
  loopOverConstraints mempty 1 d
  where
    loopOverConstraints :: (TCM types m) => MetaSet -> Int -> Maybe (NormalisableDecl types) -> m ()
    loopOverConstraints recentlySolvedMetas loopNumber decl = do
      unsolvedConstraints <- getActiveConstraints @types

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
                trackSolvedMetas (Proxy @types) $
                  runUnificationSolver (Proxy @types) recentlySolvedMetas

              logUnsolvedUnknowns updatedDecl (Just recentlySolvedMetas)

              metasSolvedDuringInstanceResolution <-
                trackSolvedMetas (Proxy @types) $
                  runInstanceSolver (Proxy @types) metasSolvedDuringUnification
              return metasSolvedDuringInstanceResolution

            loopOverConstraints newMetasSolved (loopNumber + 1) updatedDecl

-- | Attempts to solve as many type-class constraints as possible. Takes in
-- the set of meta-variables solved since the solver was last run and outputs
-- the set of meta-variables solved during this run.
runInstanceSolver :: forall types m. (TCM types m) => Proxy types -> MetaSet -> m ()
runInstanceSolver _ metasSolved =
  logCompilerPass MaxDetail ("instance solver run" <> line) $
    runConstraintSolver @types
      getActiveTypeClassConstraints
      setTypeClassConstraints
      solveInstance
      metasSolved

-- | Attempts to solve as many unification constraints as possible. Takes in
-- the set of meta-variables solved since unification was last run and outputs
-- the set of meta-variables solved during this run.
runUnificationSolver :: forall types m. (TCM types m) => Proxy types -> MetaSet -> m ()
runUnificationSolver _ metasSolved =
  logCompilerPass MaxDetail "unification solver run" $
    runConstraintSolver @types
      getActiveUnificationConstraints
      setUnificationConstraints
      solveUnificationConstraint
      metasSolved

-------------------------------------------------------------------------------
-- Unsolved constraint checks

checkAllUnknownsSolved :: (TCM types m) => Proxy types -> m ()
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

checkAllConstraintsSolved :: forall types m. (TCM types m) => Proxy types -> m ()
checkAllConstraintsSolved _ = do
  constraints <- getActiveConstraints @types
  case constraints of
    [] -> return ()
    (c : cs) -> handleTypingError $ UnsolvableConstraints (c :| cs)

checkAllMetasSolved :: (TCM types m) => Proxy types -> m ()
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

logUnsolvedUnknowns :: forall types m. (TCM types m) => Maybe (NormalisableDecl types) -> Maybe MetaSet -> m ()
logUnsolvedUnknowns maybeDecl maybeSolvedMetas = do
  whenM (loggingLevelAtLeast MaxDetail) $ do
    newSubstitution <- getMetaSubstitution @types
    updatedSubst <- substMetas newSubstitution
    logDebug MaxDetail $
      "current-solution:"
        <> line
        <> prettyVerbose (fmap unnormalised updatedSubst)
        <> line

    unsolvedMetas <- getUnsolvedMetas (Proxy @types)
    unsolvedMetasDoc <- prettyMetas (Proxy @types) unsolvedMetas
    logDebug MaxDetail $
      "unsolved-metas:"
        <> line
        <> indent 2 unsolvedMetasDoc
        <> line

    unsolvedConstraints <- getActiveConstraints @types
    case maybeSolvedMetas of
      Nothing ->
        logDebug MaxDetail $
          "unsolved-constraints:"
            <> line
            <> indent 2 (prettyVerbose unsolvedConstraints)
            <> line
      Just solvedMetas -> do
        let isUnblocked = not . constraintIsBlocked solvedMetas
        let (unblockedConstraints, blockedConstraints) = partition isUnblocked unsolvedConstraints
        logDebug MaxDetail $
          "unsolved-blocked-constraints:"
            <> line
            <> indent 2 (prettyBlockedConstraints blockedConstraints)
            <> line
        logDebug MaxDetail $
          "unsolved-unblocked-constraints:"
            <> line
            <> indent 2 (prettyVerbose unblockedConstraints)
            <> line

    case maybeDecl of
      Nothing -> return ()
      Just decl ->
        logDebug MaxDetail $
          "current-decl:"
            <> line
            <> indent 2 (prettyExternal decl)
            <> line

prettyBlockedConstraints :: (PrintableBuiltin types) => [WithContext (Constraint types)] -> Doc a
prettyBlockedConstraints constraints = do
  let pairs = fmap (\c -> prettyFriendly c <> "   " <> pretty (blockedBy $ contextOf c)) constraints
  prettySetLike pairs

bidirectionalPassDoc :: Doc a
bidirectionalPassDoc = "bidirectional pass over"

-------------------------------------------------------------------------------
-- Other

createDeclCtx :: Imports types -> TypingDeclCtx types
createDeclCtx imports =
  Map.fromList $
    [getEntry d | imp <- imports, let Main ds = imp, d <- ds]
  where
    getEntry :: GluedDecl types -> (Identifier, TypingDeclCtxEntry types)
    getEntry d = do
      let ident = identifierOf d
      (ident, mkTypingDeclCtxEntry d)
