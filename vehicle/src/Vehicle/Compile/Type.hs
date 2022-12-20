
module Vehicle.Compile.Type
  ( getNormalised
  , getUnnormalised
  , getGlued
  , typeCheck
  , typeCheckExpr
  , getPropertyInfo
  ) where

import Control.Monad (filterM, forM, unless, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..))
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map (fromList)
import Data.Maybe (mapMaybe)

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Auxiliary
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.ConstraintSolver.TypeClass
import Vehicle.Compile.Type.ConstraintSolver.TypeClassDefaults
import Vehicle.Compile.Type.ConstraintSolver.Unification
import Vehicle.Compile.Type.Generalise
import Vehicle.Compile.Type.Irrelevance
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Resource
import Vehicle.Compile.Type.VariableContext (TypingDeclCtx, TypingDeclCtxEntry,
                                             toDeclCtxEntry)
import Vehicle.Expr.Normalised

-------------------------------------------------------------------------------
-- Algorithm

typeCheck :: MonadCompile m
          => ImportedModules
          -> UncheckedProg
          -> m TypedProg
typeCheck imports uncheckedProg =
  logCompilerPass MinDetail "type checking" $
    runTypeCheckerT (createDeclCtx imports) $
      typeCheckProg uncheckedProg

typeCheckExpr :: MonadCompile m
              => ImportedModules
              -> UncheckedExpr
              -> m CheckedExpr
typeCheckExpr imports expr1 =
  runTypeCheckerT (createDeclCtx imports) $ do
    expr2 <- insertHolesForAuxiliaryAnnotations expr1
    (expr3, _exprType) <- runReaderT (inferExpr expr2) mempty
    solveConstraints Nothing
    expr4 <- substMetas expr3
    expr5 <- removeIrrelevantCode expr4
    checkAllUnknownsSolved
    return expr5

-------------------------------------------------------------------------------
-- Type-class for things that can be type-checked

typeCheckProg :: TCM m => UncheckedProg -> m TypedProg
typeCheckProg (Main ds) = Main <$> typeCheckDecls ds

typeCheckDecls :: TCM m => [UncheckedDecl] -> m [TypedDecl]
typeCheckDecls = \case
  []     -> return []
  d : ds -> do
    typedDecl <- typeCheckDecl d
    checkedDecls <- addDeclContext typedDecl $ typeCheckDecls ds
    return $ typedDecl : checkedDecls

typeCheckDecl :: TCM m => UncheckedDecl -> m TypedDecl
typeCheckDecl uncheckedDecl =
  logCompilerPass MaxDetail ("declaration" <+> quotePretty (identifierOf uncheckedDecl)) $ do
    -- First insert any missing auxiliary arguments into the decl
    auxDecl <- insertHolesForAuxiliaryAnnotations uncheckedDecl

    gluedDecl <- case auxDecl of
      DefPostulate p n t     -> typeCheckPostulate p n t
      DefResource  p n r t   -> typeCheckResource  p n r t
      DefFunction  p n b r t -> typeCheckFunction  p n b r t

    checkAllUnknownsSolved
    finalDecl <- substMetas gluedDecl
    logCompilerPassOutput $ prettySimple (fmap unnormalised finalDecl)

    return $ fmap TypedExpr finalDecl

typeCheckPostulate :: TCM m
                   => Provenance
                   -> Identifier
                   -> UncheckedType
                   -> m GluedDecl
typeCheckPostulate p ident typ  = do
  checkedType <- checkDeclType ident typ
  gluedType <- glueNBE 0 checkedType
  return $ DefPostulate p ident gluedType

typeCheckResource :: TCM m
                  => Provenance
                  -> Identifier
                  -> Resource
                  -> UncheckedType
                  -> m GluedDecl
typeCheckResource p ident resource uncheckedType = do
  checkedType <- checkDeclType ident uncheckedType
  let checkedDecl = DefResource p ident resource checkedType
  solveConstraints (Just checkedDecl)
  substCheckedType <- substMetas checkedType

  -- Add extra constraints from the resource type. Need to have called
  -- solve constraints beforehand in order to allow for normalisation,
  -- but really only need to have solved type-class constraints.
  gluedType <- glueNBE 0 substCheckedType
  updatedCheckedType <- checkResourceType resource (ident, p) gluedType
  let updatedCheckedDecl = DefResource p ident resource updatedCheckedType
  solveConstraints (Just updatedCheckedDecl)

  substDecl <- substMetas updatedCheckedDecl
  logUnsolvedUnknowns (Just substDecl) Nothing

  finalDecl <- generaliseOverUnsolvedMetaVariables substDecl
  gluedDecl <- traverse (glueNBE 0) finalDecl
  return gluedDecl

typeCheckFunction :: TCM m
                  => Provenance
                  -> Identifier
                  -> Bool
                  -> UncheckedType
                  -> UncheckedExpr
                  -> m GluedDecl
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

  if isProperty then do
    gluedDecl <- traverse (glueNBE 0) substDecl
    checkPropertyType (fmap TypedExpr gluedDecl)
    return gluedDecl
  else do
    -- Otherwise if not a property then generalise over unsolved meta-variables.
    checkedDecl1 <- addFunctionAuxiliaryInputOutputConstraints substDecl
    logUnsolvedUnknowns (Just substDecl) Nothing

    checkedDecl2 <- generaliseOverUnsolvedConstraints checkedDecl1
    checkedDecl3 <- generaliseOverUnsolvedMetaVariables checkedDecl2
    gluedDecl <- traverse (glueNBE 0) checkedDecl3
    return gluedDecl

checkDeclType :: TCM m => Identifier -> UncheckedExpr -> m CheckedType
checkDeclType ident declType = do
  let pass = bidirectionalPassDoc <+> "type of" <+> quotePretty ident
  logCompilerPass MidDetail pass $ do
    (checkedType, typeOfType) <- runReaderT (inferExpr declType) mempty
    assertDeclTypeIsType ident typeOfType
    return checkedType

assertDeclTypeIsType :: TCM m => Identifier -> CheckedType -> m ()
-- This is a bit of a hack to get around having to have a solver for universe
-- levels. As type definitions will always have an annotated Type 0 inserted
-- by delaboration, we can match on it here. Anything else will be unified
-- with type 0.
assertDeclTypeIsType _     TypeUniverse{} = return ()
assertDeclTypeIsType ident actualType     = do
  let p = provenanceOf actualType
  let expectedType = TypeUniverse p 0
  let origin = CheckingExprType (FreeVar p ident) expectedType actualType
  addFreshUnificationConstraint TypeGroup p mempty origin expectedType actualType
  return ()

-------------------------------------------------------------------------------
-- Constraint solving

-- | Tries to solve constraints. Passes in the type of the current declaration
-- being checked, as metas are handled different according to whether they
-- occur in the type or not.
solveConstraints :: TCM m => Maybe CheckedDecl -> m ()
solveConstraints decl = logCompilerPass MinDetail "constraint solving" $ do
  loopOverConstraints 1 decl

loopOverConstraints :: TCM m => Int -> Maybe CheckedDecl -> m ()
loopOverConstraints loopNumber decl = do
  unsolvedConstraints <- getUnsolvedConstraints

  unless (null unsolvedConstraints) $ do
    metasSolvedLastLoop <- getAndClearRecentlySolvedMetas
    let isUnblocked = not . constraintIsBlocked metasSolvedLastLoop
    let (unblockedConstraints, blockedConstraints) = partition isUnblocked unsolvedConstraints

    if null unblockedConstraints then do
      -- If no constraints are unblocked then try generating new constraints using defaults.
      successfullyGeneratedDefault <- addNewConstraintUsingDefaults decl
      when successfullyGeneratedDefault $
        -- If new constraints generated then continue solving.
        loopOverConstraints loopNumber decl

    else do
      -- If we have made useful progress then start a new pass
      updatedDecl <- logCompilerPass MaxDetail
        ("constraint solving pass" <+> pretty loopNumber) $ do

        updatedDecl <- traverse substMetas decl
        logUnsolvedUnknowns updatedDecl (Just metasSolvedLastLoop)

        setConstraints blockedConstraints
        mconcat `fmap` traverse solveConstraint unblockedConstraints

        newSubstitution <- getMetaSubstitution
        whenM (loggingLevelAtLeast MaxDetail) $ do
          updatedSubst <- substMetas newSubstitution
          logDebug MaxDetail $ "current-solution:" <+>
            prettyVerbose (fmap unnormalised updatedSubst) <> "\n"

        return updatedDecl

      loopOverConstraints (loopNumber + 1) updatedDecl

-- | Tries to solve a constraint deterministically.
solveConstraint :: TCM m => WithContext Constraint -> m ()
solveConstraint unnormConstraint = do
  WithContext constraint ctx <- substConstraintMetas unnormConstraint

  logCompilerSection MaxDetail ("trying" <+> prettyVerbose constraint) $ do
    result <- case constraint of
      UnificationConstraint c -> solveUnificationConstraint (WithContext c ctx)
      TypeClassConstraint   c -> solveTypeClassConstraint   (WithContext c ctx)

    case result of
      Progress newConstraints -> addConstraints newConstraints
      Stuck metas -> do
        let blockedConstraint = blockConstraintOn (WithContext constraint ctx) metas
        addConstraints [blockedConstraint]

-- | Tries to add new unification constraints using default values.
addNewConstraintUsingDefaults :: TCM m
                              => Maybe CheckedDecl
                              -> m Bool
addNewConstraintUsingDefaults maybeDecl = do
  logDebug MaxDetail $ "Temporarily stuck" <> line

  logCompilerPass MidDetail
    "trying to generate a new constraint using type-classes defaults" $ do

    -- Calculate the set of candidate constraints
    candidateConstraints <- getDefaultCandidates maybeDecl
    logDebug MaxDetail $ "Candidate type-class constraints:" <> line <>
      indent 2 (prettyVerbose candidateConstraints) <> line

    result <- generateConstraintUsingDefaults candidateConstraints
    case result of
      Nothing            -> return False
      Just newConstraint -> do
        addConstraints [newConstraint]
        return True

getDefaultCandidates :: TCM m => Maybe CheckedDecl -> m [WithContext TypeClassConstraint]
getDefaultCandidates maybeDecl = do
  constraints <- getUnsolvedConstraints
  let typeClassConstraints = mapMaybe getTypeClassConstraint constraints
  case maybeDecl of
    Nothing   -> return typeClassConstraints
    Just decl -> do
      declType <- substMetas (typeOf decl)

      -- We only want to generate default solutions for constraints
      -- that *don't* appear in the type of the declaration, as those will be
      -- quantified over later.
      typeMetas <- getMetasLinkedToMetasIn constraints isTypeUniverse declType

      whenM (loggingLevelAtLeast MaxDetail) $ do
        unsolvedMetasInTypeDoc <- prettyMetas typeMetas
        logDebug MaxDetail $
          "Metas transitively related to type-signature:" <+> unsolvedMetasInTypeDoc

      flip filterM typeClassConstraints $ \tc -> do
        constraintMetas <- metasIn (objectIn tc)
        return $ MetaSet.disjoint constraintMetas typeMetas

-------------------------------------------------------------------------------
-- Property information extraction

checkPropertyType :: TCM m => TypedDecl -> m ()
checkPropertyType property = do
  _ <- getPropertyInfo property
  return ()

getPropertyInfo :: MonadCompile m => TypedDecl -> m PropertyInfo
getPropertyInfo property = do
  propertyInfo <- go (normalised (glued $ typeOf property))
  return propertyInfo
  where
  go :: MonadCompile m => NormType -> m PropertyInfo
  go = \case
    VTensorType _ tElem _ -> go tElem
    VVectorType _ tElem _ -> go tElem
    VAnnBoolType _ (VLinearityExpr _ lin) (VPolarityExpr _ pol) ->
      return $ PropertyInfo lin pol
    _                     -> do
      let declProv = (identifierOf property, provenanceOf property)
      throwError $ PropertyTypeUnsupported declProv (glued $ typeOf property)

-------------------------------------------------------------------------------
-- Unsolved constraint checks

checkAllUnknownsSolved :: TCM m => m ()
checkAllUnknownsSolved = do
  -- First check all user constraints (i.e. unification and type-class
  -- constraints) are solved.
  checkAllConstraintsSolved
  -- Then check all meta-variables have been solved.
  checkAllMetasSolved
  -- Then clear the meta-ctx
  clearMetaCtx

checkAllConstraintsSolved :: TCM m => m ()
checkAllConstraintsSolved = do
  constraints <- getUnsolvedConstraints
  case constraints of
    []       -> return ()
    (c : cs) -> throwError $ UnsolvedConstraints (c :| cs)

checkAllMetasSolved :: TCM m => m ()
checkAllMetasSolved = do
  unsolvedMetas <- getUnsolvedMetas
  case MetaSet.toList unsolvedMetas of
    []     -> return ()
    m : ms -> do
      metasAndOrigins <- forM (m :| ms) (\meta -> do
        origin <- getMetaProvenance meta
        return (meta, origin))
      throwError $ UnsolvedMetas metasAndOrigins

logUnsolvedUnknowns :: TCM m => Maybe CheckedDecl -> Maybe MetaSet -> m ()
logUnsolvedUnknowns maybeDecl maybeSolvedMetas = do
  whenM (loggingLevelAtLeast MaxDetail) $ do
    unsolvedMetas    <- getUnsolvedMetas
    unsolvedMetasDoc <- prettyMetas unsolvedMetas
    logDebug MaxDetail $ "unsolved-metas:" <> line <>
      indent 2 unsolvedMetasDoc <> line

    unsolvedConstraints <- getUnsolvedConstraints
    substUnsolvedConstraints <- traverse substConstraintMetas unsolvedConstraints
    case maybeSolvedMetas of
      Nothing ->
        logDebug MaxDetail $ "unsolved-constraints:" <> line <>
          indent 2 (prettyVerbose substUnsolvedConstraints) <> line
      Just solvedMetas -> do
        let isUnblocked = not . constraintIsBlocked solvedMetas
        let (unblockedConstraints, blockedConstraints) = partition isUnblocked substUnsolvedConstraints
        logDebug MaxDetail $ "unsolved-blocked-constraints:" <> line <>
          indent 2 (prettyVerbose blockedConstraints) <> line
        logDebug MaxDetail $ "unsolved-unblocked-constraints:" <> line <>
          indent 2 (prettyVerbose unblockedConstraints) <> line

    case maybeDecl of
      Nothing   -> return ()
      Just decl -> logDebug MaxDetail $ "current-decl:" <> line <>
        indent 2 (prettyVerbose decl) <> line

bidirectionalPassDoc :: Doc a
bidirectionalPassDoc = "bidirectional pass over"

-------------------------------------------------------------------------------
-- Interface for unnormalised code

-- | Retrieves a normalised representation of the typed expression that has
-- been simplified as much as possible.
-- All irrelevent code (such as polarity and linearity annotations) is also
-- removed.
getNormalised :: MonadCompile m => TypedExpr -> m NormExpr
getNormalised expr = removeIrrelevantCode (normalised (glued expr))

-- | Retrieves an unnormalised representation of the typed expression
-- that should be roughly equivalent to what the user wrote.
-- All irrelevent code (such as polarity and linearity annotations) is also
-- removed.
getUnnormalised :: MonadCompile m => TypedExpr -> m CheckedExpr
getUnnormalised expr = removeIrrelevantCode (unnormalised (glued expr))

getGlued :: MonadCompile m => TypedExpr -> m GluedExpr
getGlued expr = removeIrrelevantCode (glued expr)

-------------------------------------------------------------------------------
-- Other

createDeclCtx :: ImportedModules -> TypingDeclCtx
createDeclCtx imports = Map.fromList $
  [ getEntry d | imp <- imports, let Main ds = imp, d <- ds]
  where
    getEntry :: TypedDecl -> (Identifier, TypingDeclCtxEntry)
    getEntry d = do
      let ident = identifierOf d
      (ident, toDeclCtxEntry d)
