
module Vehicle.Compile.Type
  ( TypeCheckable(..)
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty(..))

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.Print
import Vehicle.Compile.Type.Unify
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.MetaSet qualified as MetaSet
import Vehicle.Compile.Type.TypeClass
import Vehicle.Compile.Type.TypeClass.Defaults
import Vehicle.Compile.Type.Auxiliary
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.WeakHeadNormalForm
import Vehicle.Compile.Type.VariableContext
import Vehicle.Compile.Type.Resource
import Vehicle.Compile.Type.Generalise

-------------------------------------------------------------------------------
-- Algorithm

class TypeCheckable a b where
  typeCheck :: MonadCompile m => a -> m b

instance TypeCheckable UncheckedProg CheckedProg where
  typeCheck prog1 =
    logCompilerPass MinDetail "type checking" $ runTCM $ do
    prog2 <- typeCheckProg prog1
    prog3 <- postProcess prog2
    logDebug MaxDetail $ prettyFriendlyDBClosed prog3
    return prog3


instance TypeCheckable UncheckedExpr CheckedExpr where
  typeCheck expr1 = runTCM $ do
    expr2 <- insertHolesForAuxiliaryAnnotations expr1
    (expr3, _exprType) <- inferExpr expr2
    solveConstraints Nothing
    expr4 <- postProcess expr3
    checkAllUnknownsSolved
    return expr4

postProcess :: ( TCM m
               , MetaSubstitutable a
               , WHNFable a
               , RemoveAuxiliaryArguments a
               )
            => a -> m a
postProcess x = do
  metaFreeExpr <- substMetas x
  normExpr     <- convertImplicitArgsToWHNF metaFreeExpr
  finalExpr    <- removeAuxiliaryArguments normExpr
  return finalExpr

-------------------------------------------------------------------------------
-- Type-class for things that can be type-checked

typeCheckProg :: TCM m => UncheckedProg -> m CheckedProg
typeCheckProg (Main ds) = Main <$> typeCheckDecls ds

typeCheckDecls :: TCM m => [UncheckedDecl] -> m [CheckedDecl]
typeCheckDecls [] = return []
typeCheckDecls (d : ds) = do
  -- First insert any missing auxiliary arguments into the decl
  d' <- insertHolesForAuxiliaryAnnotations d
  checkedDecl  <- typeCheckDecl d'
  checkedDecls <- addToDeclCtx checkedDecl $ typeCheckDecls ds
  return $ checkedDecl : checkedDecls

typeCheckDecl :: TCM m => UncheckedDecl -> m CheckedDecl
typeCheckDecl decl = logCompilerPass MinDetail ("declaration" <+> identDoc) $ do
  -- First run a bidirectional pass over the type of the declaration
  checkedType <- logCompilerPass MidDetail (passDoc <+> "type of" <+> identDoc) $ do
    let declType = typeOf decl
    (checkedType, typeOfType) <- inferExpr declType
    assertIsType (provenanceOf decl) typeOfType
    return checkedType

  result <- case decl of
    DefResource p r _ _ -> do
      updatedCheckedType <- checkResourceType r p ident checkedType
      let checkedDecl = DefResource p r ident updatedCheckedType

      solveConstraints (Just decl)
      substDecl <- substMetas checkedDecl
      logUnsolvedUnknowns (Just substDecl)

      finalDecl <- generaliseOverUnsolvedMetaVariables substDecl
      return finalDecl

    DefFunction p _ _ _ body -> do
      checkedBody <- logCompilerPass MidDetail (passDoc <+> "body of" <+> identDoc) $ do
        checkExpr checkedType body
      let checkedDecl = DefFunction p Nothing ident checkedType checkedBody

      solveConstraints (Just checkedDecl)

      substDecl <- substMetas checkedDecl
      logUnsolvedUnknowns (Just substDecl)

      checkedDecl2 <- generaliseOverUnsolvedTypeClassConstraints substDecl
      checkedDecl3 <- generaliseOverUnsolvedMetaVariables checkedDecl2
      finalDecl    <- updatePropertyInfo checkedDecl3
      return finalDecl

  checkAllUnknownsSolved
  logCompilerPassOutput $ prettyFriendlyDBClosed result
  return result

  where
    ident = identifierOf decl
    identDoc = squotes (pretty ident)
    passDoc = "bidirectional pass over"

assertIsType :: TCM m => Provenance -> CheckedExpr -> m ()
-- This is a bit of a hack to get around having to have a solver for universe
-- levels. As type definitions will always have an annotated Type 0 inserted
-- by delaboration, we can match on it here. Anything else will be unified
-- with type 0.
assertIsType _ (TypeUniverse _ _) = return ()
assertIsType p t        = do
  ctx <- getVariableCtx
  let typ = TypeUniverse (inserted (provenanceOf t)) 0
  addUnificationConstraint p ctx t typ
  return ()

-------------------------------------------------------------------------------
-- Constraint solving

-- | Tries to solve constraints. Passes in the type of the current declaration
-- being checked, as metas are handled different according to whether they
-- occur in the type or not.
solveConstraints :: MonadMeta m => Maybe CheckedDecl -> m ()
solveConstraints decl = logCompilerPass MinDetail "constraint solving" $ do
  constraints <- getUnsolvedConstraints
  loopOverConstraints 1 decl $ Progress
    { newConstraints = constraints
    , solvedMetas    = mempty
    }

loopOverConstraints :: MonadMeta m
                    => Int
                    -> Maybe CheckedDecl
                    -> ConstraintProgress
                    -> m ()
loopOverConstraints loopNumber decl progress = do
  constraints <- getUnsolvedConstraints

  case (constraints, progress) of
    -- If there are no outstanding constraints then halt.
    ([], _) -> return ()

    -- If no progress was made last iteration then try generating new constraints
    -- using defaults.
    (_, Stuck) -> do
      defaultProgress <- addNewConstraintUsingDefaults decl

      case defaultProgress of
        -- If still stuck then halt.
        Stuck      -> return ()
        -- If new constraints generated then continue solving.
        Progress{} -> loopOverConstraints (loopNumber + 1) decl defaultProgress

    -- If we have made useful progress then start a new pass
    (_, Progress{}) -> do

      -- TODO try to solve only either new constraints or those that contain
      -- blocking metas that were solved last iteration.
      (updatedDecl, newProgress) <- logCompilerPass MaxDetail
        ("constraint solving pass" <+> pretty loopNumber) $ do

        updatedDecl <- traverse substMetas decl
        logUnsolvedUnknowns updatedDecl

        setConstraints []
        newProgress <- mconcat `fmap` traverse solveConstraint constraints

        substMetasThroughCtx
        newSubstitution <- getMetaSubstitution
        logDebug MaxDetail $ "current-solution:" <+>
          prettyVerbose newSubstitution <> "\n"

        return (updatedDecl, newProgress)

      loopOverConstraints (loopNumber + 1) updatedDecl newProgress

-- | Tries to solve a constraint deterministically.
solveConstraint :: MonadMeta m
                => Constraint
                -> m ConstraintProgress
solveConstraint unnormConstraint = do
  constraint <- whnfConstraintWithMetas unnormConstraint

  logDebug MaxDetail $ "trying" <+> prettyVerbose constraint
  incrCallDepth

  result <- case constraint of
    UC ctx c -> solveUnificationConstraint ctx c
    TC ctx c -> solveTypeClassConstraint   ctx c

  case result of
    Progress newConstraints _ -> addConstraints newConstraints
    Stuck                     -> addConstraints [constraint]

  decrCallDepth
  return result

-- | Tries to add new unification constraints using default values.
addNewConstraintUsingDefaults :: MonadMeta m
                               => Maybe CheckedDecl
                               -> m ConstraintProgress
addNewConstraintUsingDefaults maybeDecl = do
  logDebug MaxDetail $ "Temporarily stuck" <> line

  logCompilerPass MidDetail
    "trying to generate a new constraint using type-classes defaults" $ do

    -- Calculate the set of candidate constraints
    candidateConstraints <- getDefaultCandidates maybeDecl
    logDebug MaxDetail $ "Candidate type-class constraints:" <> line <>
      indent 2 (prettySimple candidateConstraints) <> line

    result <- generateConstraintUsingDefaults candidateConstraints
    case result of
      Progress newConstraints _ -> addConstraints newConstraints
      Stuck                     -> return ()

    return result

getDefaultCandidates :: MonadMeta m => Maybe CheckedDecl -> m [Constraint]
getDefaultCandidates maybeDecl = do
  unsolvedConstraints <- filter isNonAuxiliaryTypeClassConstraint <$> getUnsolvedConstraints
  case maybeDecl of
    Nothing   -> return unsolvedConstraints
    Just decl -> do
      declType <- substMetas (typeOf decl)

      -- We only want to generate default solutions for constraints
      -- that *don't* appear in the type of the declaration, as those will be
      -- quantified over later.
      typeMetas <- getMetasLinkedToMetasIn declType isTypeUniverse

      unsolvedMetasInTypeDoc <- prettyMetas typeMetas
      logDebug MaxDetail $
        "Metas transitively related to type-signature:" <+> unsolvedMetasInTypeDoc

      return $ flip filter unsolvedConstraints $ \c ->
        MetaSet.disjoint (metasIn c) typeMetas

-------------------------------------------------------------------------------
-- Property information extraction

updatePropertyInfo :: MonadLogger m => CheckedDecl -> m CheckedDecl
updatePropertyInfo = \case
  r@DefResource{}       -> return r
  DefFunction p _ ident t e -> do
    let propertyInfo = getPropertyInfo t
    case propertyInfo of
      Nothing -> return ()
      Just (PropertyInfo linearity polarity) -> logDebug MinDetail $
        "Identified" <+> squotes (pretty ident) <+> "as a property of type:" <+>
          pretty linearity <+> pretty polarity
    return $ DefFunction p propertyInfo ident t e
  where
    getPropertyInfo :: CheckedExpr -> Maybe PropertyInfo
    getPropertyInfo = \case
      (AnnBoolType _ (Builtin _ (Linearity lin)) (Builtin _ (Polarity pol))) ->
        Just $ PropertyInfo lin pol
      (TensorType _ tElem _) -> getPropertyInfo tElem
      _                      -> Nothing

-------------------------------------------------------------------------------
-- Unsolved constraint checks

checkAllUnknownsSolved :: MonadMeta m => m ()
checkAllUnknownsSolved = do
  -- First check all user constraints (i.e. unification and type-class
  -- constraints) are solved.
  checkAllConstraintsSolved
  -- Then check all meta-variables have been solved.
  checkAllMetasSolved
  -- Then clear the meta-ctx
  clearMetaCtx

checkAllConstraintsSolved :: MonadMeta m => m ()
checkAllConstraintsSolved = do
  constraints <- getUnsolvedConstraints
  case constraints of
    []       -> return ()
    (c : cs) -> throwError $ UnsolvedConstraints (c :| cs)

checkAllMetasSolved :: MonadMeta m => m ()
checkAllMetasSolved = do
  unsolvedMetas <- getUnsolvedMetas
  case MetaSet.toList unsolvedMetas of
    []     -> return ()
    m : ms -> do
      metasAndOrigins <- forM (m :| ms) (\meta -> do
        origin <- getMetaProvenance meta
        return (meta, origin))
      throwError $ UnsolvedMetas metasAndOrigins

logUnsolvedUnknowns :: MonadMeta m => Maybe CheckedDecl -> m ()
logUnsolvedUnknowns maybeDecl = do
  unsolvedMetas    <- getUnsolvedMetas
  unsolvedMetasDoc <- prettyMetas unsolvedMetas
  logDebug MaxDetail $ "unsolved-metas:" <> line <>
    indent 2 unsolvedMetasDoc <> line

  unsolvedConstraints <- getUnsolvedConstraints
  logDebug MaxDetail $ "unsolved-constraints:" <> line <>
    indent 2 (prettyVerbose unsolvedConstraints) <> line

  case maybeDecl of
    Nothing   -> return ()
    Just decl -> logDebug MaxDetail $ "current-decl:" <> line <>
      indent 2 (prettyVerbose decl) <> line