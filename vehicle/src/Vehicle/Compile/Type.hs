module Vehicle.Compile.Type
  ( typeCheck,
    typeCheckExpr,
    runUnificationSolver,
    getPropertyInfo,
    getNormalised,
    getUnnormalised,
    getGlued,
  )
where

import Control.Monad (filterM, forM, unless, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..))
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map (fromList)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (InternalMonadNorm (..), NormT, NormalisableBuiltin, runEmptyNormT)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Constraint.Core (runConstraintSolver)
import Vehicle.Compile.Type.Constraint.UnificationSolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Generalise
import Vehicle.Compile.Type.Irrelevance
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Substitution
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem (TypableBuiltin (..))
import Vehicle.Compile.Type.VariableContext
  ( TypingDeclCtx,
    TypingDeclCtxEntry,
    toDeclCtxEntry,
  )
import Vehicle.Expr.Normalised

-------------------------------------------------------------------------------
-- Algorithm

typeCheck ::
  (TypableBuiltin builtin, MonadCompile m) =>
  Imports builtin ->
  UncheckedProg builtin ->
  m (TypedProg builtin)
typeCheck imports uncheckedProg =
  logCompilerPass MinDetail "type checking" $
    runTypeChecker (createDeclCtx imports) $ do
      typeCheckProg uncheckedProg

typeCheckExpr ::
  forall builtin m.
  (TypableBuiltin builtin, MonadCompile m) =>
  Imports builtin ->
  UncheckedExpr builtin ->
  m (CheckedExpr builtin)
typeCheckExpr imports expr1 =
  runTypeChecker (createDeclCtx imports) $ do
    expr2 <- insertHolesForAuxAnnotations expr1
    (expr3, _exprType) <- runReaderT (inferExpr expr2) mempty
    solveConstraints @builtin Nothing
    expr4 <- substMetas expr3
    expr5 <- removeIrrelevantCode expr4
    checkAllUnknownsSolved (Proxy @builtin)
    return expr5

-------------------------------------------------------------------------------
-- Type-class for things that can be type-checked

typeCheckProg :: TCM builtin m => UncheckedProg builtin -> m (TypedProg builtin)
typeCheckProg (Main ds) = Main <$> typeCheckDecls ds

typeCheckDecls :: TCM builtin m => [UncheckedDecl builtin] -> m [TypedDecl builtin]
typeCheckDecls = \case
  [] -> return []
  d : ds -> do
    typedDecl <- typeCheckDecl d
    checkedDecls <- addDeclContext typedDecl $ typeCheckDecls ds
    return $ typedDecl : checkedDecls

typeCheckDecl :: forall builtin m. TCM builtin m => UncheckedDecl builtin -> m (TypedDecl builtin)
typeCheckDecl uncheckedDecl =
  logCompilerPass MaxDetail ("declaration" <+> quotePretty (identifierOf uncheckedDecl)) $ do
    -- First insert any missing auxiliary arguments into the decl
    auxDecl <- traverse insertHolesForAuxAnnotations uncheckedDecl

    gluedDecl <- case auxDecl of
      DefPostulate p n t -> typeCheckPostulate p n t
      DefResource p n r t -> typeCheckResource p n r t
      DefFunction p n b r t -> typeCheckFunction p n b r t

    checkAllUnknownsSolved (Proxy @builtin)
    finalDecl <- substMetas gluedDecl
    logCompilerPassOutput $ prettyExternal (fmap unnormalised finalDecl)

    return $ fmap StandardTypedExpr finalDecl

typeCheckPostulate ::
  TCM builtin m =>
  Provenance ->
  Identifier ->
  UncheckedType builtin ->
  m (GluedDecl builtin)
typeCheckPostulate p ident typ = do
  checkedType <- checkDeclType ident typ
  gluedType <- glueNBE 0 checkedType
  return $ DefPostulate p ident gluedType

typeCheckResource ::
  TCM builtin m =>
  Provenance ->
  Identifier ->
  Resource ->
  UncheckedType builtin ->
  m (GluedDecl builtin)
typeCheckResource p ident resource uncheckedType = do
  checkedType <- checkDeclType ident uncheckedType
  let checkedDecl = DefResource p ident resource checkedType
  solveConstraints (Just checkedDecl)
  substCheckedType <- substMetas checkedType

  -- Add extra constraints from the resource type. Need to have called
  -- solve constraints beforehand in order to allow for normalisation,
  -- but really only need to have solved type-class constraints.
  gluedType <- glueNBE 0 substCheckedType
  updatedCheckedType <- typeResource resource (ident, p) gluedType
  let updatedCheckedDecl = DefResource p ident resource updatedCheckedType
  solveConstraints (Just updatedCheckedDecl)

  substDecl <- substMetas updatedCheckedDecl
  logUnsolvedUnknowns (Just substDecl) Nothing

  finalDecl <- generaliseOverUnsolvedMetaVariables substDecl
  gluedDecl <- traverse (glueNBE 0) finalDecl
  return gluedDecl

typeCheckFunction ::
  TCM builtin m =>
  Provenance ->
  Identifier ->
  Bool ->
  UncheckedType builtin ->
  UncheckedExpr builtin ->
  m (GluedDecl builtin)
typeCheckFunction p ident isProperty typ body = do
  checkedType <- checkDeclType ident typ

  -- Type check the body.
  let pass = bidirectionalPassDoc <+> "body of" <+> quotePretty ident
  checkedBody <- logCompilerPass MidDetail pass $ do
    runReaderT (checkExpr checkedType body) mempty

  -- Reconstruct the function.
  let checkedDecl = DefFunction p ident isProperty checkedType checkedBody

  -- Solve constraints and substitute through.
  solveConstraints (Just checkedDecl)
  substDecl <- substMetas checkedDecl

  if isProperty
    then do
      gluedDecl <- traverse (glueNBE 0) substDecl
      checkPropertyType (fmap StandardTypedExpr gluedDecl)
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
      gluedDecl <- traverse (glueNBE 0) checkedDecl3
      return gluedDecl

checkDeclType :: TCM builtin m => Identifier -> UncheckedExpr builtin -> m (CheckedType builtin)
checkDeclType ident declType = do
  let pass = bidirectionalPassDoc <+> "type of" <+> quotePretty ident
  logCompilerPass MidDetail pass $ do
    (checkedType, typeOfType) <- runReaderT (inferExpr declType) mempty
    assertDeclTypeIsType ident typeOfType
    return checkedType

assertDeclTypeIsType :: TCM builtin m => Identifier -> CheckedType builtin -> m ()
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
solveConstraints :: forall builtin m. TCM builtin m => Maybe (CheckedDecl builtin) -> m ()
solveConstraints d = logCompilerPass MinDetail "constraint solving" $ do
  loopOverConstraints mempty 1 d
  where
    loopOverConstraints :: TCM builtin m => MetaSet -> Int -> Maybe (CheckedDecl builtin) -> m ()
    loopOverConstraints recentlySolvedMetas loopNumber decl = do
      unsolvedConstraints <- getActiveConstraints @builtin

      updatedDecl <- traverse substMetas decl
      logUnsolvedUnknowns updatedDecl (Just recentlySolvedMetas)

      unless (null unsolvedConstraints) $ do
        let allConstraintsBlocked = all (constraintIsBlocked recentlySolvedMetas) unsolvedConstraints

        if allConstraintsBlocked
          then do
            -- If no constraints are unblocked then try generating new constraints using defaults.
            successfullyGeneratedDefault <- addNewConstraintUsingDefaults decl
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
              logDebug MaxDetail line
              metasSolvedDuringInstanceResolution <-
                trackSolvedMetas (Proxy @builtin) $
                  runInstanceSolver (Proxy @builtin) metasSolvedDuringUnification
              return metasSolvedDuringInstanceResolution

            loopOverConstraints newMetasSolved (loopNumber + 1) updatedDecl

-- | Tries to add new unification constraints using default values.
addNewConstraintUsingDefaults ::
  TCM builtin m =>
  Maybe (CheckedDecl builtin) ->
  m Bool
addNewConstraintUsingDefaults maybeDecl = do
  logDebug MaxDetail $ "Temporarily stuck" <> line

  logCompilerPass
    MidDetail
    "trying to generate a new constraint using type-classes defaults"
    $ do
      -- Calculate the set of candidate constraints
      candidateConstraints <- getDefaultCandidates maybeDecl
      logDebug MaxDetail $
        "Candidate type-class constraints:"
          <> line
          <> indent 2 (prettyVerbose candidateConstraints)
          <> line

      result <- generateDefaultConstraint candidateConstraints
      case result of
        Nothing -> return False
        Just newConstraint -> do
          addConstraints [newConstraint]
          return True

getDefaultCandidates :: forall builtin m. TCM builtin m => Maybe (CheckedDecl builtin) -> m [WithContext (TypeClassConstraint builtin)]
getDefaultCandidates maybeDecl = do
  typeClassConstraints <- getActiveTypeClassConstraints
  case maybeDecl of
    Nothing -> return typeClassConstraints
    Just decl -> do
      declType <- substMetas (typeOf decl)

      -- We only want to generate default solutions for constraints
      -- that *don't* appear in the type of the declaration, as those will be
      -- quantified over later.
      constraints <- getActiveConstraints
      typeMetas <- getMetasLinkedToMetasIn constraints isTypeUniverse declType

      whenM (loggingLevelAtLeast MaxDetail) $ do
        unsolvedMetasInTypeDoc <- prettyMetas (Proxy @builtin) typeMetas
        logDebug MaxDetail $
          "Metas transitively related to type-signature:" <+> unsolvedMetasInTypeDoc

      flip filterM typeClassConstraints $ \tc -> do
        constraintMetas <- metasIn (objectIn tc)
        return $ MetaSet.disjoint constraintMetas typeMetas

-- | Attempts to solve as many type-class constraints as possible. Takes in
-- the set of meta-variables solved since the solver was last run and outputs
-- the set of meta-variables solved during this run.
runInstanceSolver :: forall builtin m. TCM builtin m => Proxy builtin -> MetaSet -> m ()
runInstanceSolver _ metasSolved =
  logCompilerPass MaxDetail ("instance solver run" <> line) $
    runConstraintSolver @builtin
      getActiveTypeClassConstraints
      setTypeClassConstraints
      solveInstance
      metasSolved

-- | Attempts to solve as many unification constraints as possible. Takes in
-- the set of meta-variables solved since unification was last run and outputs
-- the set of meta-variables solved during this run.
runUnificationSolver :: forall builtin m. TCM builtin m => Proxy builtin -> MetaSet -> m ()
runUnificationSolver _ metasSolved =
  logCompilerPass MaxDetail "unification solver run" $
    runConstraintSolver @builtin
      getActiveUnificationConstraints
      setUnificationConstraints
      solveUnificationConstraint
      metasSolved

-------------------------------------------------------------------------------
-- Property information extraction

checkPropertyType :: TCM builtin m => TypedDecl builtin -> m ()
checkPropertyType property = do
  _ <- getPropertyInfo property
  return ()

-------------------------------------------------------------------------------
-- Unsolved constraint checks

checkAllUnknownsSolved :: TCM builtin m => Proxy builtin -> m ()
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

checkAllConstraintsSolved :: forall builtin m. TCM builtin m => Proxy builtin -> m ()
checkAllConstraintsSolved _ = do
  constraints <- getActiveConstraints @builtin
  case constraints of
    [] -> return ()
    (c : cs) -> handleTypingError $ UnsolvableConstraints (c :| cs)

checkAllMetasSolved :: TCM builtin m => Proxy builtin -> m ()
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

logUnsolvedUnknowns :: forall builtin m. TCM builtin m => Maybe (CheckedDecl builtin) -> Maybe MetaSet -> m ()
logUnsolvedUnknowns maybeDecl maybeSolvedMetas = do
  whenM (loggingLevelAtLeast MaxDetail) $ do
    newSubstitution <- getMetaSubstitution @builtin
    updatedSubst <- substMetas newSubstitution
    logDebug MaxDetail $
      "current-solution:"
        <> line
        <> prettyVerbose (fmap unnormalised updatedSubst)
        <> line

    unsolvedMetas <- getUnsolvedMetas (Proxy @builtin)
    unsolvedMetasDoc <- prettyMetas (Proxy @builtin) unsolvedMetas
    logDebug MaxDetail $
      "unsolved-metas:"
        <> line
        <> indent 2 unsolvedMetasDoc
        <> line

    unsolvedConstraints <- getActiveConstraints @builtin
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
            <> indent 2 (prettyVerbose decl)
            <> line

prettyBlockedConstraints :: PrintableBuiltin builtin => [WithContext (Constraint builtin)] -> Doc a
prettyBlockedConstraints constraints = do
  let pairs = fmap (\c -> prettyFriendly c <> "   " <> pretty (blockedBy $ contextOf c)) constraints
  prettySetLike pairs

bidirectionalPassDoc :: Doc a
bidirectionalPassDoc = "bidirectional pass over"

-------------------------------------------------------------------------------
-- Interface for unnormalised code

-- | Retrieves a normalised representation of the typed expression that has
-- been simplified as much as possible.
-- All irrelevent code (such as polarity and linearity annotations) is also
-- removed.
getNormalised :: forall builtin m. (NormalisableBuiltin builtin, MonadCompile m) => TypedExpr builtin -> m (NormExpr builtin)
getNormalised expr = runEmptyNormT $ removeIrrelevantCode @(NormT builtin _) (normalised (glued expr))

-- | Retrieves an unnormalised representation of the typed expression
-- that should be roughly equivalent to what the user wrote.
-- All irrelevent code (such as polarity and linearity annotations) is also
-- removed.
getUnnormalised :: MonadCompile m => TypedExpr builtin -> m (CheckedExpr builtin)
getUnnormalised expr = removeIrrelevantCode (unnormalised (glued expr))

getGlued :: forall builtin m. (NormalisableBuiltin builtin, MonadCompile m) => TypedExpr builtin -> m (GluedExpr builtin)
getGlued expr = runEmptyNormT $ removeIrrelevantCode @(NormT builtin _) (glued expr)

-------------------------------------------------------------------------------
-- Other

createDeclCtx :: Imports builtin -> TypingDeclCtx builtin
createDeclCtx imports =
  Map.fromList $
    [getEntry d | imp <- imports, let Main ds = imp, d <- ds]
  where
    getEntry :: TypedDecl builtin -> (Identifier, TypingDeclCtxEntry builtin)
    getEntry d = do
      let ident = identifierOf d
      (ident, toDeclCtxEntry d)
