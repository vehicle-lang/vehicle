
module Vehicle.Compile.Type
  ( TypeCheckable(..)
  ) where

import Prelude hiding (pi)
import Control.Monad.Except (MonadError(..))
import Control.Monad ( forM, foldM )
import Data.Foldable (fold)
import Data.List (partition)
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
import Vehicle.Compile.Type.Auxiliary.Polarity (solvePolarityConstraint)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.WeakHeadNormalForm
import Vehicle.Compile.Resource
import Vehicle.Compile.Normalise as Norm

-------------------------------------------------------------------------------
-- Algorithm

class TypeCheckable a b where
  typeCheck :: MonadCompile m => a -> m b

instance TypeCheckable UncheckedProg CheckedProg where
  typeCheck prog1 = logCompilerPass "type checking" $ runTCM $ do
    prog2 <- preProcess prog1
    prog3 <- typeCheckProg prog2
    prog4 <- postProcess prog3
    logDebug MaxDetail $ prettyFriendlyDBClosed prog4
    return prog4

instance TypeCheckable UncheckedExpr CheckedExpr where
  typeCheck expr1 = runTCM $ do
    expr2 <- preProcess expr1
    (expr3, _exprType) <- inferExpr expr2
    solveConstraints Nothing
    checkAllConstraintsSolved
    postProcess expr3

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
  checkedDecl  <- typeCheckDecl d
  checkedDecls <- addToDeclCtx checkedDecl $ typeCheckDecls ds
  return $ checkedDecl : checkedDecls

typeCheckDecl :: TCM m => UncheckedDecl -> m CheckedDecl
typeCheckDecl decl = do
  let ident = identifierOf decl
  let identDoc = squotes (pretty ident)
  let passDoc = "bidirectional pass over"

  logCompilerPass ("declaration" <+> identDoc) $ do

    -- First run a bidirectional pass over the type of the declaration
    checkedType <- logCompilerPass (passDoc <+> "type of" <+> identDoc) $ do
      let declType = typeOf decl
      (checkedType, typeOfType) <- inferExpr declType
      assertIsType (annotationOf decl) typeOfType
      return checkedType

    let solveConstraintsAndUpdateType = do
          solveConstraints (Just checkedType)
          unsolvedMetas <- getUnsolvedMetas
          logDebug MaxDetail $ "unsolved-metas:" <+> pretty unsolvedMetas
          unsolvedConstraints <- getUnsolvedConstraints
          logDebug MaxDetail $ "unsolved-constraints:" <+> prettyVerbose unsolvedConstraints <> line
          substMetas checkedType

    result <- case decl of
      DefResource p r _ _ -> do
        substType <- solveConstraintsAndUpdateType
        declCtx <- getDeclCtx
        whnfType <- normalise substType $ defaultNormalisationOptions
          { Norm.declContext = declCtx
          }

        checkResourceType r p ident whnfType
        let checkedDecl = DefResource p r ident substType
        return checkedDecl

      DefFunction p _ _ _ body -> do
        checkedBody <- logCompilerPass (passDoc <+> "body of" <+> identDoc) $ do
          checkExpr checkedType body

        substType <- solveConstraintsAndUpdateType
        substBody <- substMetas checkedBody

        (finalType, finalBody) <- handleUnsolvedDeclConstraints (substType, substBody)

        -- Extract property info if the declaration is a property.
        let propertyInfo = getPropertyInfo finalType
        case propertyInfo of
          Nothing -> return ()
          Just (PropertyInfo polarity) -> logDebug MinDetail $
            "Identified" <+> identDoc <+> "as a property of type:" <+> pretty (Polarity polarity)

        let checkedDecl = DefFunction p propertyInfo ident finalType finalBody
        return checkedDecl

    checkAllConstraintsSolved
    logCompilerPassOutput $ prettyVerbose result
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

-- Tries to solve constraints. Passes in the type of the current declaration
-- being checked, as metas are handled different according to whether they
-- occur in the type or not.
solveConstraints :: MonadConstraintSolving m => Maybe CheckedExpr -> m ()
solveConstraints declType = logCompilerPass "constraint solving" $ do
  constraints <- getUnsolvedConstraints
  loopOverConstraints declType $ Progress
    { newConstraints = constraints
    , solvedMetas    = mempty
    }

loopOverConstraints :: MonadConstraintSolving m
                    => Maybe CheckedExpr
                    -> ConstraintProgress
                    -> m ()
loopOverConstraints maybeDeclType progress = do
  constraints <- getUnsolvedConstraints

  case (constraints, progress) of
    -- If there are no outstanding constraints then halt.
    ([], _) -> return ()

    -- If no progress was made last iteration then try generating new constraints
    -- using defaults.
    (_, Stuck) -> do
      defaultProgress <- addNewConstraintsUsingDefaults maybeDeclType

      case defaultProgress of
        -- If still stuck then halt.
        Stuck      -> return ()
        -- If new constraints generated then continue solving.
        Progress{} -> loopOverConstraints maybeDeclType defaultProgress

    -- If we have made useful progress then start a new pass
    (_, Progress{}) -> do

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

      loopOverConstraints maybeDeclType newProgress

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
                               => Maybe CheckedExpr
                               -> m ConstraintProgress
addNewConstraintsUsingDefaults maybeDeclType = do
  logDebug MaxDetail "Temporarily stuck - trying to generate new constraints using defaults for type-classes"
  incrCallDepth

  -- Calculate the set of metas to try to generate default solutions for...
  unsolvedMetas <- getUnsolvedMetas
  metasToGenerateDefaultsFor <- MetaSet.toList <$> case maybeDeclType of
    Nothing       -> return unsolvedMetas
    Just declType -> do
      -- In particular we only want to generate default solutions for constraints
      -- that *don't* appear in the type of the declaration, as those will be
      -- quantified over later.
      metasInType <- freeMetas <$> substMetas declType

      logDebug MaxDetail $ "Unsolved metas:" <+> pretty unsolvedMetas
      logDebug MaxDetail $ "Unsolved metas in type-signature:" <+> pretty metasInType

      return $ MetaSet.difference unsolvedMetas metasInType

  logDebug MaxDetail $ "Unsolved metas to generate defaults for:" <+> pretty metasToGenerateDefaultsFor

  constraints <- filter isTypeClassConstraint <$> getUnsolvedConstraints
  logDebug MaxDetail $ "Unsolved type-class constraints:" <> line <> indent 2 (prettySimple constraints) <> line

  result <- fold <$> forM metasToGenerateDefaultsFor generateDefaultTypeClassSolution
  case result of
    Progress newConstraints _ -> addConstraints newConstraints
    Stuck                     -> return ()

  decrCallDepth
  return result

-------------------------------------------------------------------------------
-- Auxiliary constraints

handleUnsolvedDeclConstraints :: MonadConstraintSolving m
                              => (CheckedExpr, CheckedExpr)
                              -> m (CheckedExpr, CheckedExpr)
handleUnsolvedDeclConstraints decl@(declType, _) = do
  -- Remove all non-unification constraints and append them to the front
  -- of the declaration.
  (unificationConstraints, nonUnificationConstraints) <-
    partition isUnificationConstraint <$> getUnsolvedConstraints

  constrainedDecl <- if null nonUnificationConstraints
    then return decl
    else do
      logDebug MaxDetail "Prepending constraints to declaration type:"
      incrCallDepth
      result <- foldM prependConstraint decl nonUnificationConstraints
      decrCallDepth
      logDebug MaxDetail ""
      return result
  setConstraints unificationConstraints

  -- Quantify over any unsolved type-level meta variables
  let unsolvedMetas = freeMetas declType
  quantifiedDecl  <- if MetaSet.null unsolvedMetas
    then return constrainedDecl
    else do
      logDebug MaxDetail "Adding unsolved metas in declaration type as implicit arguments:"
      incrCallDepth
      result <- foldM quantifyOverMeta constrainedDecl (MetaSet.toList unsolvedMetas)
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

  prependBinder Instance typeClass substDecl

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
  else do
    let ann = annotationOf (fst decl)
    let solution = Var ann (Bound 0)
    metaSolved meta solution

    subst <- getMetaSubstitution
    logDebug MaxDetail $ prettyVerbose subst

    substDecl <- substMetas decl
    prependBinder Implicit substMetaType substDecl

prependBinder :: MonadCompile m
              => Visibility
              -> CheckedExpr
              -> (CheckedExpr, CheckedExpr)
              -> m (CheckedExpr, CheckedExpr)
prependBinder v binderType (declType, declBody) = do
  let ann1 = annotationOf declType
  let ann2 = annotationOf declBody
  let binder1 = Binder ann1 v Nothing binderType
  let binder2 = Binder ann2 v Nothing binderType
  let resultType = Pi  ann1 binder1 declType
  let resultBody = Lam ann2 binder2 declBody
  logCompilerPassOutput $ prettyVerbose resultType <+> prettyVerbose resultBody
  return (resultType, resultBody)

-------------------------------------------------------------------------------
-- Property information extraction

getPropertyInfo :: CheckedExpr -> Maybe PropertyInfo
getPropertyInfo (AnnotatedBoolType _ p) = Just $ PropertyInfo p
getPropertyInfo (TensorType _ tElem _)  = getPropertyInfo tElem
getPropertyInfo _                       = Nothing

-------------------------------------------------------------------------------
-- Unsolved constraint checks

checkAllConstraintsSolved :: MonadConstraintSolving m => m ()
checkAllConstraintsSolved = do
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

checkAllUserConstraintsSolved :: MonadConstraintSolving m => m ()
checkAllUserConstraintsSolved = do
  constraints <- filter (not . isAuxiliaryConstraint) <$> getUnsolvedConstraints
  case constraints of
    []       -> return ()
    (c : cs) -> throwError $ UnsolvedConstraints (c :| cs)

checkAllMetasSolved :: MonadConstraintSolving m => m ()
checkAllMetasSolved = do
  unsolvedMetas <- getUnsolvedMetas
  case MetaSet.toList unsolvedMetas of
    []     -> return ()
    m : ms -> do
      metasAndOrigins <- forM (m :| ms) (\meta -> do
        (origin, _) <- getMetaInfo meta
        return (meta, origin))
      throwError $ UnsolvedMetas metasAndOrigins

checkAllAuxiliaryConstraintsSolved :: MonadConstraintSolving m => m ()
checkAllAuxiliaryConstraintsSolved = do
  auxConstraints <- filter isAuxiliaryConstraint <$> getUnsolvedConstraints
  case auxConstraints of
    [] -> return ()
    _  -> compilerDeveloperError $
      "Unsolved auxiliary constraints - supposed to be impossible:" <+>
      prettySimple auxConstraints


