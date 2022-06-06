
module Vehicle.Compile.Type
  ( TypeCheckable(..)
  ) where

import Prelude hiding (pi)
import Control.Monad.Except (MonadError(..))
import Control.Monad ( forM, foldM )
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty(..))

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.Print
import Vehicle.Compile.Type.Unify
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.TypeClass
import Vehicle.Compile.Type.Auxiliary
import Vehicle.Compile.Type.Auxiliary.Polarity (solvePolarityConstraint)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Defaults
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.WeakHeadNormalForm

-------------------------------------------------------------------------------
-- Algorithm

class TypeCheckable a b where
  typeCheck :: MonadCompile m => a -> m b

instance TypeCheckable UncheckedProg CheckedProg where
  typeCheck p = logCompilerPass "type checking" $ runTCM $ do
    p' <- preProcess p
    result <- typeCheckProg p'
    r <- postProcess result
    logDebug MaxDetail $ prettyFriendlyDBClosed r
    return r

instance TypeCheckable UncheckedExpr CheckedExpr where
  typeCheck expr = runTCM $ do
    expr' <- preProcess expr
    (checkedExpr, _checkedExprType) <- inferExpr expr'
    solveConstraints
    postProcess checkedExpr

preProcess :: ( TCM m
              , InsertAuxiliaryAnnotations a
              ) => a -> m a
preProcess x =
  return $ insertHolesForAuxiliaryAnnotations x

postProcess :: ( TCM m
               , MetaSubstitutable a
               , WHNFable a
               , RemoveAuxiliaryArguments a
               )
            => a -> m a
postProcess x = do
  -- First check all user constraints (i.e. unification and type-class
  -- constraints) are solved.
  checkAllUserConstraintsSolved
  -- Then check all meta-variables have been solved.
  checkAllMetasSolved
  -- Then as a sanity check, check that all auxiliary constraints have
  -- been solved. In theory the only way they can't have been solved is
  -- if there are unsolved metas, e.g. the domain of `hasQuantifier`
  -- constraint.
  checkAllAuxiliaryConstraintsSolved

  metaFreeExpr <- substMetas x
  normExpr <- convertImplicitArgsToWHNF metaFreeExpr

  -- Remove all auxiliary constraint related code from the result.
  finalExpr <- removeAuxiliaryArguments normExpr
  return finalExpr

-------------------------------------------------------------------------------
-- Type-class for things that can be type-checked

typeCheckProg :: TCM m => UncheckedProg -> m CheckedProg
typeCheckProg (Main ds) = Main <$> typeCheckDecls ds

typeCheckDecls :: TCM m => [UncheckedDecl] -> m [CheckedDecl]
typeCheckDecls [] = return []
typeCheckDecls (d : ds) = do
  -- Check the current declaration.
  (checkedDecl, checkedDeclBody, checkedDeclType) <- typeCheckDecl d

  -- Recursively check the remainder of the declarations.
  checkedDecls <- addToDeclCtx (identifierOf d) checkedDeclType checkedDeclBody $
    typeCheckDecls ds

  return $ checkedDecl : checkedDecls

typeCheckDecl :: TCM m => UncheckedDecl -> m (CheckedDecl, Maybe CheckedExpr, CheckedExpr)
typeCheckDecl d = do
  let ident = identifierOf d
  let identDoc = squotes (pretty ident)
  let passDoc = "bidirectional pass over"

  logCompilerPass ("declaration" <+> identDoc) $ do

    -- First run a bidirectional pass over the type of the declaration
    checkedType <- logCompilerPass (passDoc <+> "type of" <+> identDoc) $ do
      let declType = typeOf d
      (checkedType, typeOfType) <- inferExpr declType
      assertIsType (annotationOf d) typeOfType
      return checkedType

    -- Check the body (if present)

    let solveConstraintsAndUpdateType = do
          solveConstraints
          unsolvedMetas <- getUnsolvedMetas
          logDebug MaxDetail $ "unsolved-metas:" <+> pretty unsolvedMetas
          unsolvedConstraints <- getConstraints
          logDebug MaxDetail $ "unsolved-constraints:" <+> prettyVerbose unsolvedConstraints <> line
          substMetas checkedType

    result@(resultDecl, _, _) <- case d of
      DefResource p r _ _ -> do
        substType <- solveConstraintsAndUpdateType
        let checkedDecl = DefResource p r ident substType
        return (checkedDecl, Nothing, substType)

      DefFunction p usage _ _ body -> do
        checkedBody <- logCompilerPass (passDoc <+> "body of" <+> identDoc) $ do
          checkExpr checkedType body

        substType <- solveConstraintsAndUpdateType
        substBody <- substMetas checkedBody

        (finalType, finalBody) <- handleUnsolvedDeclConstraints (substType, substBody)
        --let (finalType, finalBody) = (substType, substBody)

        let checkedDecl = DefFunction p usage ident finalType finalBody
        return (checkedDecl, Just finalBody, finalType)

    logCompilerPassOutput $ prettyVerbose resultDecl

    return result

assertIsType :: TCM m => Provenance -> CheckedExpr -> m ()
-- This is a bit of a hack to get around having to have a solver for universe
-- levels. As type definitions will always have an annotated Type 0 inserted
-- by delaboration, we can match on it here. Anything else will be unified
-- with type 0.
assertIsType _ (Type _ _) = return ()
assertIsType p t        = do
  ctx <- getVariableCtx
  let typ = Type (inserted (provenanceOf t)) 0
  addUnificationConstraint p ctx t typ
  return ()

-------------------------------------------------------------------------------
-- Constraint solving

solveConstraints :: MonadConstraintSolving m => m ()
solveConstraints = logCompilerPass "constraint solving" $ do
  constraints <- getConstraints
  loopOverConstraints $ Progress
    { newConstraints = constraints
    , solvedMetas    = mempty
    }

loopOverConstraints :: MonadConstraintSolving m
                    => ConstraintProgress
                    -> m ()
loopOverConstraints progress = do
  constraints <- getConstraints

  case (constraints, progress) of
    -- If there are no outstanding constraints then halt.
    ([], _) -> return ()

    -- Likewise halt if no progress was made last iteration.
    (_, Stuck) -> return ()

    -- If we have made useful progress then start a new pass
    (_, Progress _newConstraints _solvedMetas) ->do

      -- TODO try to solve only either new constraints or those that contain
      -- blocking metas that were solved last iteration.
      updatedConstraints <- substMetas constraints

      logDebug MaxDetail "Starting new constraint solving pass"
      logDebug MaxDetail $ "current-constraints:" <+>
        align (prettyVerbose updatedConstraints)

      setConstraints []
      newProgress <- mconcat `fmap` traverse solveConstraint updatedConstraints

      newSubstitution <- getMetaSubstitution
      logDebug MaxDetail $ "current-solution:" <+>
        prettyVerbose newSubstitution <> "\n"

      loopOverConstraints newProgress

-- | Tries to solve a constraint deterministically.
solveConstraint :: MonadConstraintSolving m
                => Constraint
                -> m ConstraintProgress
solveConstraint constraint = do
  logDebug MaxDetail $ "trying" <+> prettyVerbose constraint
  incrCallDepth

  result <- case constraint of
    UC ctx c -> solveUnificationConstraint ctx c
    TC ctx c -> solveTypeClassConstraint   ctx c
    PC ctx c -> solvePolarityConstraint    ctx c

  case result of
    Progress newConstraints _ -> addConstraints newConstraints
    Stuck                     -> addConstraints [constraint]

  decrCallDepth
  return result

-- | Tries to add new unification constraints using default values.
addNewConstraintsUsingDefaults :: MonadConstraintSolving m
                               => m ConstraintProgress
addNewConstraintsUsingDefaults = do
  logDebug MaxDetail "Temporarily stuck - trying default type-class constraints"
  incrCallDepth

  constraints <- getTypeClassConstraints

  result <- solveDefaultTypeClassConstraints constraints
  case result of
    Progress newConstraints _ -> addConstraints newConstraints
    Stuck                     -> return ()

  logDebug MaxDetail line
  decrCallDepth
  return result

-------------------------------------------------------------------------------
-- Auxiliary constraint insertion

handleUnsolvedDeclConstraints :: MonadConstraintSolving m
                              => (CheckedExpr, CheckedExpr)
                              -> m (CheckedExpr, CheckedExpr)
handleUnsolvedDeclConstraints decl@(declType, _) = do
  -- Remove all non-unification constraints and append them to the front
  -- of the declaration.
  (auxiliaryConstraints, otherConstraints) <-
    partition isAuxiliaryConstraint <$> getConstraints

  constrainedDecl <- if null auxiliaryConstraints
    then return decl
    else do
      logDebug MaxDetail "Prepending constraints to declaration type:"
      incrCallDepth
      result <- foldM prependConstraint decl auxiliaryConstraints
      decrCallDepth
      logDebug MaxDetail ""
      return result
  setConstraints otherConstraints

  -- Quantify over any unsolved type-level meta variables
  let unsolvedMetas = freeMetas declType
  quantifiedDecl  <- if null unsolvedMetas
    then return constrainedDecl
    else do
      logDebug MaxDetail "Adding unsolved metas in declaration type as implicit arguments:"
      incrCallDepth
      result <- foldM quantifyOverMeta constrainedDecl unsolvedMetas
      decrCallDepth
      logDebug MaxDetail ""
      return result

  traverse substMetas quantifiedDecl

prependConstraint :: MonadConstraintSolving m
                  => (CheckedExpr, CheckedExpr)
                  -> Constraint
                  -> m (CheckedExpr, CheckedExpr)
prependConstraint decl constraint = do
  logDebug MaxDetail $ prettySimple constraint

  (typeClass, maybeMeta) <- case constraint of
    TC _ (meta `Has` t) -> return (t, Just meta)
    PC _ c              -> return (c, Nothing)
    UC{}                -> compilerDeveloperError
      "Unification constraints should have been filtered out earlier"

  substDecl <- case maybeMeta of
    Nothing   -> return decl
    Just meta -> do
      let ann = annotationOf (fst decl)
      metaSolved meta (Var ann (Bound 0))
      substMetas decl

  prependBinder typeClass substDecl

quantifyOverMeta :: MonadConstraintSolving m
                 => (CheckedExpr, CheckedExpr)
                 -> Meta
                 -> m (CheckedExpr, CheckedExpr)
quantifyOverMeta decl meta = do
  (_, metaType) <- getMetaInfo meta
  substMetaType <- substMetas metaType
  if isMeta substMetaType
    then compilerDeveloperError $
      "Haven't thought about what to do when type of unsolved meta is also" <+>
      "an unsolved meta."
  else if not $ isAuxiliaryType substMetaType
    then return decl
  else do
    let ann = annotationOf (fst decl)
    let solution = Var ann (Bound 0)
    metaSolved meta solution

    subst <- getMetaSubstitution
    logDebug MaxDetail $ prettyVerbose subst

    substDecl <- substMetas decl
    prependBinder substMetaType substDecl

prependBinder :: MonadCompile m
              => CheckedExpr
              -> (CheckedExpr, CheckedExpr)
              -> m (CheckedExpr, CheckedExpr)
prependBinder binderType (declType, declBody) = do
  let ann1 = annotationOf declType
  let ann2 = annotationOf declBody
  let binder1 = ImplicitBinder ann1 Nothing binderType
  let binder2 = ImplicitBinder ann2 Nothing binderType
  let resultType = Pi  ann1 binder1 declType
  let resultBody = Lam ann2 binder2 declBody
  logCompilerPassOutput $ prettyVerbose resultType <+> prettyVerbose resultBody
  return (resultType, resultBody)

-------------------------------------------------------------------------------
-- Checks

checkAllUserConstraintsSolved :: MonadConstraintSolving m => m ()
checkAllUserConstraintsSolved = do
  constraints <- filter (not . isAuxiliaryConstraint) <$> getConstraints
  case constraints of
    [] -> return ()
    (c : cs) -> do
      result <- addNewConstraintsUsingDefaults
      case result of
        Progress _ _ -> do
          loopOverConstraints result
          checkAllUserConstraintsSolved
        Stuck                     -> do
          logDebug MaxDetail "Still stuck"
          throwError $ UnsolvedConstraints (c :| cs)

checkAllMetasSolved :: MonadConstraintSolving m => m ()
checkAllMetasSolved = do
  unsolvedMetas <- getUnsolvedMetas
  case unsolvedMetas of
    []     -> return ()
    m : ms -> do
      metasAndOrigins <- forM (m :| ms) (\meta -> do
        (origin, _) <- getMetaInfo meta
        return (meta, origin))
      throwError $ UnsolvedMetas metasAndOrigins

checkAllAuxiliaryConstraintsSolved :: MonadConstraintSolving m => m ()
checkAllAuxiliaryConstraintsSolved = do
  auxConstraints <- filter isAuxiliaryConstraint <$> getConstraints
  case auxConstraints of
    [] -> return ()
    _  -> compilerDeveloperError $
      "Unsolved auxiliary constraints - supposed to be impossible:" <+>
      prettySimple auxConstraints
