
module Vehicle.Compile.Type
  ( TypeCheckable(..)
  ) where

import Prelude hiding (pi)
import Control.Monad.Except (MonadError(..))
import Control.Monad (forM)
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
    postProcess result

instance TypeCheckable UncheckedExpr CheckedExpr where
  typeCheck expr = runTCM $ do
    expr' <- preProcess expr
    (checkedExpr, _checkedExprType) <- inferExpr expr'
    solveConstraints
    postProcess checkedExpr

-------------------------------------------------------------------------------
-- Logging

showDeclEntry :: MonadLogger m => Identifier -> m ()
showDeclEntry ident = do
  logDebug MaxDetail ("decl-entry" <+> pretty ident)
  incrCallDepth

showDeclExit :: MonadLogger m => Identifier -> m ()
showDeclExit ident = do
  decrCallDepth
  logDebug MaxDetail ("decl-exit" <+> pretty ident)

-------------------------------------------------------------------------------
-- Type-class for things that can be type-checked

typeCheckProg :: TCM m => UncheckedProg -> m CheckedProg
typeCheckProg (Main ds) = Main <$> typeCheckDecls ds

typeCheckDecls :: TCM m => [UncheckedDecl] -> m [CheckedDecl]
typeCheckDecls [] = return []
typeCheckDecls (d : ds) = do
  let ident = identifierOf d
  showDeclEntry ident

  (checkedDecl, checkedDeclBody, checkedDeclType, typeOfType) <- case d of
    DefResource p r _ t -> do
      (checkedType, typeOfType) <- inferExpr t
      let checkedDecl = DefResource p r ident checkedType
      return (checkedDecl, Nothing, checkedType, typeOfType)

    DefFunction p usage _ t body -> do
      (checkedType, typeOfType) <- inferExpr t
      checkedBody <- checkExpr checkedType body
      let checkedDecl = DefFunction p usage ident checkedType checkedBody
      return (checkedDecl, Just checkedBody, checkedType, typeOfType)

  assertIsType (annotationOf d) typeOfType
  logDebug MinDetail ""
  solveConstraints

  showDeclExit ident

  -- Recursively check the remainder of the declarations
  checkedDecls <- addToDeclCtx ident checkedDeclType checkedDeclBody $
    typeCheckDecls ds

  return $ checkedDecl : checkedDecls

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

  substitution <- getMetaSubstitution
  let metaFreeExpr = substMetas substitution x
  normExpr <- convertImplicitArgsToWHNF metaFreeExpr

  -- Remove all auxiliary constraint related code from the result.
  let finalExpr = removeAuxiliaryArguments normExpr
  return finalExpr

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
  substitution <- getMetaSubstitution

  case (constraints, progress) of
    -- If there are no outstanding constraints then halt.
    ([], _) -> return ()

    -- Likewise halt if no progress was made last iteration.
    (_, Stuck) -> return ()

    -- If we have made useful progress then start a new pass
    (_, Progress _newConstraints _solvedMetas) ->do

      -- TODO try to solve only either new constraints or those that contain
      -- blocking metas that were solved last iteration.
      let updatedConstraints = substMetas substitution constraints

      logDebug MaxDetail "Starting new pass"
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
      metasAndOrigins <- forM (m :| ms) (\v -> do
        let meta = MetaVar v
        origin <- getMetaOrigin meta
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
