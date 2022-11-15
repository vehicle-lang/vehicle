
module Vehicle.Compile.Type
  ( typeCheck
  , typeCheckExpr
  ) where

import Control.Monad ( forM, unless, when, filterM )
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Writer (MonadWriter (..), runWriterT)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map (singleton)
import Data.Set qualified as Set (member)

import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Auxiliary
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.ConstraintSolver.TypeClass
import Vehicle.Compile.Type.ConstraintSolver.TypeClassDefaults
import Vehicle.Compile.Type.ConstraintSolver.Unification
import Vehicle.Compile.Type.Generalise
import Vehicle.Compile.Type.Irrelevance
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.MetaSet qualified as MetaSet
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Resource
import Vehicle.Language.Print
import Vehicle.Compile.Normalise.NormExpr (GluedExpr(..), GluedProg, GluedDecl)

-------------------------------------------------------------------------------
-- Algorithm

typeCheck :: MonadCompile m
          => UncheckedProg
          -> UncheckedPropertyContext
          -> m (GluedProg, PropertyContext)
typeCheck uncheckedProg uncheckedCtx =
  logCompilerPass MinDetail "type checking" $ runTypeCheckerT $ do
    (checkedProg, checkedCtx) <- runWriterT $ typeCheckProg uncheckedCtx uncheckedProg
    cleanedProg <- postProcess checkedProg
    logDebug MaxDetail $ prettyFriendlyDBClosed (fmap unnormalised cleanedProg)
    return (cleanedProg, checkedCtx)

typeCheckExpr :: MonadCompile m => UncheckedExpr -> m CheckedExpr
typeCheckExpr expr1 = runTypeCheckerT $ do
  expr2 <- insertHolesForAuxiliaryAnnotations expr1
  (expr3, _exprType) <- runReaderT (inferExpr expr2) mempty
  solveConstraints Nothing
  expr4 <- postProcess expr3
  checkAllUnknownsSolved
  return expr4

postProcess :: ( TCM m
               , MetaSubstitutable a
               , RemoveIrrelevantCode a
               )
            => a -> m a
postProcess x = do
  metaFreeExpr <- substMetas x
  finalExpr    <- removeIrrelevantCode metaFreeExpr
  return finalExpr

-------------------------------------------------------------------------------
-- Type-class for things that can be type-checked

type TopLevelTCM m =
  ( TCM m
  , MonadWriter PropertyContext m
  )

typeCheckProg :: TopLevelTCM m => UncheckedPropertyContext -> UncheckedProg -> m GluedProg
typeCheckProg ctx (Main ds) = Main <$> typeCheckDecls ctx ds

typeCheckDecls :: TopLevelTCM m => UncheckedPropertyContext -> [UncheckedDecl] -> m [GluedDecl]
typeCheckDecls _   [] = return []
typeCheckDecls ctx (d : ds) = do
  -- First insert any missing auxiliary arguments into the decl
  d' <- insertHolesForAuxiliaryAnnotations d
  checkedDecl  <- typeCheckDecl ctx d'
  gluedDecl <- traverse (glueNBE 0) checkedDecl
  checkedDecls <- addDeclContext gluedDecl $ typeCheckDecls ctx ds
  return $ gluedDecl : checkedDecls

typeCheckDecl :: TopLevelTCM m => UncheckedPropertyContext -> UncheckedDecl -> m CheckedDecl
typeCheckDecl propertyCtx decl = do
  let ident = identifierOf decl
  let identDoc = quotePretty ident
  let passDoc = "bidirectional pass over"

  logCompilerPass MinDetail ("declaration" <+> identDoc) $ do
    -- First run a bidirectional pass over the type of the declaration
    checkedType <- logCompilerPass MidDetail (passDoc <+> "type of" <+> identDoc) $ do
      let declType = typeOf decl
      (checkedType, typeOfType) <- runReaderT (inferExpr declType) mempty
      assertDeclTypeIsType ident typeOfType
      return checkedType

    result <- case decl of
      DefResource p r _ _ -> do
        let checkedDecl = DefResource p r ident checkedType
        solveConstraints (Just checkedDecl)
        substCheckedType <- substMetas checkedType

        -- Add extra constraints from the resource type. Need to have called
        -- solve constraints beforehand in order to allow for normalisation,
        -- but really only need to have solved type-class constraints.
        gluedType <- glueNBE 0 substCheckedType
        updatedCheckedType <- checkResourceType r (ident, p) gluedType
        let updatedCheckedDecl = DefResource p r ident updatedCheckedType
        solveConstraints (Just updatedCheckedDecl)

        substDecl <- substMetas updatedCheckedDecl
        logUnsolvedUnknowns (Just substDecl) Nothing

        finalDecl <- generaliseOverUnsolvedMetaVariables substDecl
        return finalDecl

      DefPostulate p _ _ -> do
        return $ DefPostulate p ident checkedType

      DefFunction p _ _ body -> do
        -- Type check the body.
        checkedBody <- logCompilerPass MidDetail (passDoc <+> "body of" <+> identDoc) $ do
          runReaderT (checkExpr checkedType body) mempty

        -- Reconstruct the function.
        let checkedDecl = DefFunction p ident checkedType checkedBody

        -- Solve constraints and substitute through.
        solveConstraints (Just checkedDecl)
        substDecl <- substMetas checkedDecl

        -- Extract auxiliary annotations if a property.
        -- This check must happen before generalisation as the `Bool` type will get
        -- generalised with function input/output constraints.
        let isProperty = ident `Set.member` propertyCtx
        when isProperty $ do
          checkPropertyInfo (ident, p) (typeOf substDecl)

        checkedDecl1 <- addFunctionAuxiliaryInputOutputConstraints substDecl
        logUnsolvedUnknowns (Just substDecl) Nothing

        checkedDecl2 <- generaliseOverUnsolvedTypeClassConstraints checkedDecl1
        checkedDecl3 <- generaliseOverUnsolvedMetaVariables checkedDecl2
        return checkedDecl3

    checkAllUnknownsSolved
    logCompilerPassOutput $ prettyFriendlyDBClosed result
    return result

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
  metasSolvedLastLoop <- getSolvedMetas
  clearSolvedMetas

  unless (null unsolvedConstraints) $ do
    let isUnblocked = isUnblockedBy metasSolvedLastLoop
    let (unblockedConstraints, blockedConstraints) = partition isUnblocked unsolvedConstraints

    if null unblockedConstraints then do
      -- If no constraints are unblocked then try generating new constraints using defaults.
      successfullyGeneratedDefault <- addNewConstraintUsingDefaults decl
      when successfullyGeneratedDefault $ do
        -- If new constraints generated then continue solving.
        loopOverConstraints loopNumber decl

    else do
      -- If we have made useful progress then start a new pass

      -- TODO try to solve only either new constraints or those that contain
      -- blocking metas that were solved last iteration.
      updatedDecl <- logCompilerPass MaxDetail
        ("constraint solving pass" <+> pretty loopNumber) $ do

        updatedDecl <- traverse substMetas decl
        logUnsolvedUnknowns updatedDecl (Just metasSolvedLastLoop)

        setConstraints blockedConstraints
        mconcat `fmap` traverse solveConstraint unblockedConstraints

        substMetasThroughCtx
        newSubstitution <- getMetaSubstitution
        logDebug MaxDetail $ "current-solution:" <+>
          prettyVerbose (fmap unnormalised newSubstitution) <> "\n"

        return updatedDecl

      loopOverConstraints (loopNumber + 1) updatedDecl

-- | Tries to solve a constraint deterministically.
solveConstraint :: TCM m => WithContext Constraint -> m ()
solveConstraint (WithContext unnormConstraint ctx) = do
  constraint <- substMetas unnormConstraint

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
      typeMetas <- getMetasLinkedToMetasIn declType isTypeUniverse

      unsolvedMetasInTypeDoc <- prettyMetas typeMetas
      logDebug MaxDetail $
        "Metas transitively related to type-signature:" <+> unsolvedMetasInTypeDoc

      flip filterM typeClassConstraints $ \tc -> do
        constraintMetas <- metasIn (objectIn tc)
        return $ MetaSet.disjoint constraintMetas typeMetas

-------------------------------------------------------------------------------
-- Property information extraction

checkPropertyInfo :: TopLevelTCM m => DeclProvenance -> CheckedType -> m ()
checkPropertyInfo decl@(ident, _) t = do
  propertyInfo <- getPropertyInfo t
  tell (Map.singleton ident propertyInfo)
  logDebug MinDetail $
    "Identified" <+> squotes (pretty ident) <+> "as a property of type:" <+> pretty propertyInfo

  where
    getPropertyInfo :: MonadCompile m => CheckedType -> m PropertyInfo
    getPropertyInfo = \case
      AnnBoolType _ (LinearityExpr _ lin) (PolarityExpr _ pol) -> return $ PropertyInfo lin pol
      VectorType _ tElem _ -> getPropertyInfo tElem
      TensorType _ tElem _ -> getPropertyInfo tElem
      otherType            -> throwError $ PropertyTypeUnsupported decl otherType

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
  unsolvedMetas    <- getUnsolvedMetas
  unsolvedMetasDoc <- prettyMetas unsolvedMetas
  logDebug MaxDetail $ "unsolved-metas:" <> line <>
    indent 2 unsolvedMetasDoc <> line

  unsolvedConstraints <- getUnsolvedConstraints
  case maybeSolvedMetas of
    Nothing ->
      logDebug MaxDetail $ "unsolved-constraints:" <> line <>
        indent 2 (prettyVerbose unsolvedConstraints) <> line
    Just solvedMetas -> do
      let isUnblocked = isUnblockedBy solvedMetas
      let (unblockedConstraints, blockedConstraints) = partition isUnblocked unsolvedConstraints
      logDebug MaxDetail $ "unsolved-blocked-constraints:" <> line <>
        indent 2 (prettyVerbose blockedConstraints) <> line
      logDebug MaxDetail $ "unsolved-unblocked-constraints:" <> line <>
        indent 2 (prettyVerbose unblockedConstraints) <> line

  case maybeDecl of
    Nothing   -> return ()
    Just decl -> logDebug MaxDetail $ "current-decl:" <> line <>
      indent 2 (prettyVerbose decl) <> line
