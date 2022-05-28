
module Vehicle.Compile.Type
  ( typeCheck
  ) where

import Prelude hiding (pi)
import Control.Monad.Except (MonadError(..))
import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty(..))
import Data.IntSet qualified as IntSet

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.Print
import Vehicle.Compile.Type.Unify
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.TypeClass
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Defaults
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.MetaSubstitution (metasIn)
import Vehicle.Compile.Type.WeakHeadNormalForm

-------------------------------------------------------------------------------
-- Algorithm

typeCheck :: ( MonadCompile m
             , TypeCheckable a b
             , MetaSubstitutable b
             , WHNFable b
             , PrettyWith ('As 'Internal) b
             ) => a -> m b
typeCheck e = logCompilerPass "type checking" $ runTCM $ do
  inferredExpr     <- tc e
  solveConstraints
  checkAllConstraintsSolved
  checkAllMetasSolved

  metaSubstitution <- getMetaSubstitution
  let metaFreeExpr = substMetas metaSubstitution inferredExpr
  finalExpr <- convertImplicitArgsToWHNF metaFreeExpr
  return finalExpr

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

class TypeCheckable a b where
  tc :: TCM m => a -> m b

instance TypeCheckable UncheckedProg CheckedProg where
  tc (Main ds) = Main <$> tc ds

instance TypeCheckable [UncheckedDecl] [CheckedDecl] where
  tc []       = return []
  tc (d : ds) = do
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

    showDeclExit ident
    checkedDecls <- addToDeclCtx ident checkedDeclType checkedDeclBody $ tc ds
    return $ checkedDecl : checkedDecls

instance TypeCheckable UncheckedExpr CheckedExpr where
  tc e = fst <$> inferExpr e

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

checkAllConstraintsSolved :: MonadConstraintSolving m => m ()
checkAllConstraintsSolved = do
  constraints <- getConstraints
  case constraints of
    [] -> return ()
    (c : cs) -> do
      result <- addNewConstraintsUsingDefaults
      case result of
        Progress _ _ -> do
          loopOverConstraints result
          checkAllConstraintsSolved
        Stuck                     -> do
          logDebug MaxDetail "Still stuck"
          throwError $ UnsolvedConstraints (c :| cs)

checkAllMetasSolved :: MonadConstraintSolving m => m ()
checkAllMetasSolved = do
  metasSolved  <- metasIn <$> getMetaSubstitution
  metasCreated <- (\v -> IntSet.fromList [0..v-1]) <$> numberOfMetasCreated
  let unsolvedMetas = IntSet.difference metasCreated metasSolved
  case IntSet.toList unsolvedMetas of
    []     -> return ()
    m : ms -> do
      metasAndOrigins <- forM (m :| ms) (\v -> do
        let meta = MetaVar v
        origin <- getMetaOrigin meta
        return (meta, origin))
      throwError $ UnsolvedMetas metasAndOrigins