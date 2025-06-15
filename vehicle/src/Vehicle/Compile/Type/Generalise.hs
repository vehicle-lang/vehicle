{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list literal" #-}
module Vehicle.Compile.Type.Generalise
  ( generaliseOverUnsolvedMetasAndConstraints,
  )
where

import Control.Monad (forM, forM_, void, when)
import Data.Data (Proxy (..))
import Data.Foldable (foldlM)
import Data.Graph (graphFromEdges, topSort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
import Vehicle.Compile.Context.Bound
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote qualified as Quote
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Constraint.UnificationSolver (UnificationResult (..), unify)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Meta.Map (MetaMap)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Interface.Print (PrintableBuiltin)
import Vehicle.Data.Code.Value

--------------------------------------------------------------------------------
-- Generalisation

type MonadGeneralise builtin m =
  ( MonadTypeChecker builtin m
  )

generaliseOverUnsolvedMetasAndConstraints ::
  forall builtin m.
  (MonadGeneralise builtin m) =>
  Decl builtin ->
  m (Decl builtin)
generaliseOverUnsolvedMetasAndConstraints decl = do
  let proxy = (Proxy @builtin)
  logCompilerPass MaxDetail "generalisation over unsolved metas and constraints" $ do
    -- Check unification constraints solved
    checkAllConstraintsSolved proxy getActiveUnificationConstraints UnificationConstraint

    -- Check application constraints solved
    checkAllConstraintsSolved proxy getActiveApplicationConstraints ApplicationConstraint

    -- Remaining constraints and metas to be generalised can have no dependendies on
    -- the variables inside the term to remove them.
    dependencyFreeDecl <- removeAllDependencies decl

    -- Generalise over the
    generalisedDecl <- generaliseOverUnsolvedMetas dependencyFreeDecl

    logUnsolvedUnknowns proxy
    return generalisedDecl

--------------------------------------------------------------------------------
-- Context removal

removeAllDependencies ::
  forall builtin m.
  (MonadGeneralise builtin m) =>
  Decl builtin ->
  m (Decl builtin)
removeAllDependencies decl = do
  logCompilerPass MaxDetail "removing dependencies of unsolved metas" $ do
    -- Remove meta dependencies
    metaVariableCtx <- getMetaVariableCtx @builtin
    forM_ (MetaMap.toList metaVariableCtx) $ \(meta, metaInfo) -> do
      when (isNothing (metaSolution metaInfo) && not (null $ metaCtx metaInfo)) $
        logCompilerPass MaxDetail ("removing dependences of" <+> pretty meta) $ do
          void $ solveInTermsOfNewMetaWithDependencies meta metaInfo mempty

  logCompilerPass MaxDetail "removing dependencies from and merging instance constraints" $ do
    -- Remove instance constraint dependencies
    instanceConstraints <- getActiveInstanceConstraints @builtin
    auxiliaryInstanceConstraints <- getActiveAuxiliaryInstanceConstraints @builtin

    newInstanceConstraints <- forM instanceConstraints removeInstanceDependencies
    newAuxiliaryInstanceConstraints <- forM auxiliaryInstanceConstraints removeInstanceDependencies

    mergedInstanceConstraints <- mergeInstanceConstraints newInstanceConstraints
    mergedAuxiliaryInstanceConstraints <- mergeInstanceConstraints newAuxiliaryInstanceConstraints

    setInstanceConstraints mergedInstanceConstraints
    setAuxiliaryInstanceConstraints mergedAuxiliaryInstanceConstraints

  -- Substitute through the new metas variables through the types of the meta variables
  logCompilerPass MaxDetail "substituting metas through solution" $ do
    metaVariableCtx <- getMetaVariableCtx @builtin
    substMetaVariableCtx <- substMetas metaVariableCtx
    modifyTypeCheckerState (\s -> s {metaVariableCtx = substMetaVariableCtx})

  resultDecl <- substMetas decl
  logUnsolvedUnknowns (Proxy @builtin)
  return resultDecl

removeInstanceDependencies ::
  (MonadGeneralise builtin m) =>
  WithContext (InstanceConstraint builtin) ->
  m (WithContext (InstanceConstraint builtin))
removeInstanceDependencies c@(WithContext constraint ctx) =
  logCompilerSection MaxDetail "Removing dependencies:" $ do
    logDebug MaxDetail $ "Input: " <+> prettyExternal c
    let newCtx = updateConstraintBoundCtx ctx (const mempty)
    substConstraint <- substMetasAt (boundCtxLv $ boundContextOf ctx) constraint
    let result = WithContext substConstraint newCtx
    logDebug MaxDetail $ "Output:" <+> prettyExternal result
    return result

mergeInstanceConstraints ::
  forall builtin m.
  (MonadGeneralise builtin m) =>
  [WithContext (InstanceConstraint builtin)] ->
  m [WithContext (InstanceConstraint builtin)]
mergeInstanceConstraints constraints = do
  substitutedConstraintsByMeta <- forM constraints $ \(WithContext constraint ctx) -> do
    updatedConstraint <- updateSolutionMeta constraint
    return (instanceSolution updatedConstraint, WithContext updatedConstraint ctx :| [])

  let constraintsBySolutionMeta = MetaMap.toList $ MetaMap.fromListWith (<>) substitutedConstraintsByMeta
  mergedConstraints <- forM constraintsBySolutionMeta $ \(_meta, masterConstraint :| otherConstraints) -> do
    let getGoal = goalExpr . instanceGoal . objectIn
    let mainGoal = getGoal masterConstraint
    forM_ otherConstraints $ \otherConstraint -> do
      let secDoc = "Merging" <+> prettyExternal otherConstraint <+> "into" <> line <> prettyExternal masterConstraint
      logCompilerSection MaxDetail secDoc $ do
        result <- unify mempty mainGoal (getGoal otherConstraint)
        case result of
          Success -> return ()
          _ -> developerError "Unable to unify identical goal constraints"
    return masterConstraint

  let noMerging = all (\(_, cs) -> length cs == 1) constraintsBySolutionMeta
  if noMerging
    then return mergedConstraints
    else mergeInstanceConstraints mergedConstraints

updateSolutionMeta ::
  forall builtin m.
  (MonadGeneralise builtin m) =>
  InstanceConstraint builtin ->
  m (InstanceConstraint builtin)
updateSolutionMeta constraint = do
  let originalMeta = instanceSolution constraint
  metaCtx <- metaVariableCtx <$> getTypeCheckerState @builtin
  newMeta <- findUltimateUnsolvedMeta metaCtx originalMeta
  -- This is a hack that should disappear when we get records?
  updateMetaType newMeta (Quote.unnormalise @(Value builtin) @(Expr builtin) 0 $ goalExpr $ instanceGoal constraint)
  return $ constraint {instanceSolution = newMeta}

--------------------------------------------------------------------------------
-- Type-class generalisation

-- Finds any unsolved type class constraints that are blocked on
-- metas that occur in the type of the declaration. It then appends these
-- constraints as instance arguments to the declaration.
generaliseOverUnsolvedMetas ::
  forall builtin m.
  (MonadGeneralise builtin m) =>
  Decl builtin ->
  m (Decl builtin)
generaliseOverUnsolvedMetas decl = do
  metaVariableCtx <- getMetaVariableCtx @builtin
  let unsolvedMetas = MetaMap.filter (isNothing . metaSolution) metaVariableCtx
  sortedUnsolvedMetas <- sortGeneralisableMetas unsolvedMetas

  unsolvedInstanceConstraints <- getActiveInstanceConstraints
  unsolvedAuxInstanceConstraints <- getActiveAuxiliaryInstanceConstraints
  setInstanceConstraints @builtin mempty
  setAuxiliaryInstanceConstraints @builtin mempty

  let unsolvedConstraints = unsolvedInstanceConstraints <> unsolvedAuxInstanceConstraints
  let unsolvedConstraintMetas = MetaMap.fromList $ fmap ((\c -> (instanceSolution c, c)) . objectIn) unsolvedConstraints

  let p = provenanceOf decl
  binders <- traverse (createBinderForMeta unsolvedConstraintMetas p) (zip [1 ..] sortedUnsolvedMetas)
  generalisedDecl <- logCompilerPass MaxDetail ("generalisation over" <+> pretty sortedUnsolvedMetas) $ do
    foldlM prependBinderAndSolve decl binders
  logUnsolvedUnknowns (Proxy @builtin)
  return generalisedDecl

sortGeneralisableMetas ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  MetaVariableContext builtin ->
  m [MetaID]
sortGeneralisableMetas unsolvedMetas = do
  logCompilerPass MaxDetail "sorting generalisable constraints" $ do
    let adjacencyMap = MetaMap.map (metasIn . metaType) unsolvedMetas
    let adjacencyList = (\(x, ys) -> (x, x, MetaSet.toList ys)) <$> MetaMap.toList adjacencyMap

    let (graph, nodeFromVertex, _) = graphFromEdges adjacencyList
    let sortedVertices = topSort graph
    let sortedConstraintIDs = fmap ((\(c, _, _) -> c) . nodeFromVertex) sortedVertices
    logDebug MaxDetail $ "Adjacency matrix:" <+> pretty (fmap (\(m, _, cs) -> (m, cs)) adjacencyList)
    logDebug MaxDetail $ "Sorted order:" <+> pretty sortedConstraintIDs
    return sortedConstraintIDs

createBinderForMeta ::
  forall builtin m.
  (MonadGeneralise builtin m) =>
  MetaMap (InstanceConstraint builtin) ->
  Provenance ->
  (Int, MetaID) ->
  m (MetaID, Binder builtin)
createBinderForMeta constraints p (index, meta) = do
  metaType <- getSubstMetaType meta
  let (visibility, relevance) = case MetaMap.lookup meta constraints of
        Just constraint -> (Instance True, instanceRelevance constraint)
        Nothing -> (Implicit True, Relevant)

  -- Prepend the implicit binders for the new generalised variable.
  let binderName = "_t" <> Text.pack (show index)
  let binderDisplayForm = BinderDisplayForm (NameAndType binderName) True
  let binder = Binder p binderDisplayForm visibility relevance metaType
  return (meta, binder)

--------------------------------------------------------------------------------
-- Utilities

prependBinderAndSolve ::
  forall builtin m.
  (MonadGeneralise builtin m) =>
  Decl builtin ->
  (MetaID, Binder builtin) ->
  m (Decl builtin)
prependBinderAndSolve decl (meta, binder) =
  logCompilerPass MaxDetail ("generalising" <+> pretty meta <+> ":" <+> prettyVerbose binder) $ do
    let p = provenanceOf decl

    -- Create a new meta with dependencies on the telescope and solve the previous one in terms of it.
    metaInfo <- getMetaInfo meta
    let solutionCtx = binder : metaCtx metaInfo
    newMeta <- solveInTermsOfNewMetaWithDependencies meta metaInfo solutionCtx
    let solution = BoundVar p 0
    solveMeta newMeta solution solutionCtx

    -- Substitute the solution through the declaration (have to do this before prepending binders)
    substDecl <- substMetas decl

    -- Compute the telescopes
    let typeBinder = binder
    let bodyBinder = mapBinderNamingForm (\t -> OnlyName (fromMaybe "_" (nameOf t))) binder

    -- Then finally update the declaration
    let alterType t = return $ Pi p typeBinder t
    let alterBody e = return $ Lam p bodyBinder e
    finalDecl <- traverseDeclTypeAndExpr alterType alterBody substDecl

    -- Substitute the new meta solution through.
    setCurrentDecl $ Just (finalDecl, False)

    logCompilerPassOutput $ prettyExternal finalDecl
    return finalDecl

solveInTermsOfNewMetaWithDependencies ::
  (MonadTypeChecker builtin m) =>
  MetaID ->
  MetaInfo builtin ->
  BoundCtx (Type builtin) ->
  m MetaID
solveInTermsOfNewMetaWithDependencies meta (MetaInfo p typ _ _) newCtx = do
  (newMeta, newMetaExpr) <- freshMeta p typ newCtx
  solveMeta meta newMetaExpr newCtx
  return newMeta
