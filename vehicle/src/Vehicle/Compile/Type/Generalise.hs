{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list literal" #-}
module Vehicle.Compile.Type.Generalise
  ( generaliseOverUnsolvedMetasAndConstraints,
  )
where

import Control.Monad (forM, forM_, void)
import Data.Data (Proxy (..))
import Data.Foldable (foldlM)
import Data.Graph (graphFromEdges, topSort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
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
import Vehicle.Data.Builtin.Interface.Type (TypableBuiltin)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Variable.Bound.Context.Generic

--------------------------------------------------------------------------------
-- Generalisation

type MonadGeneralise builtin m =
  ( MonadTypeChecker builtin m,
    TypableBuiltin builtin
  )

generaliseOverUnsolvedMetasAndConstraints ::
  forall builtin m.
  (MonadGeneralise builtin m) =>
  Decl builtin ->
  m (Decl builtin)
generaliseOverUnsolvedMetasAndConstraints decl = do
  let proxy = (Proxy @builtin)
  logCompilerSection2 MaxDetail "generalisation over unsolved metas and constraints" $ do
    -- Check unification and application constraints solved
    checkAllConstraintsSolved proxy getActiveUnificationConstraints UnificationConstraint
    checkAllConstraintsSolved proxy getActiveApplicationConstraints ApplicationConstraint

    -- Remaining constraints and metas to be generalised can have no dependencies on
    -- the variables inside the term to remove them.
    dependencyFreeDecl <- removeAllDependencies decl

    -- Generalise over the unsolved metas
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
  logCompilerSection2 MaxDetail "removing dependencies of unsolved metas" $ do
    -- Remove meta dependencies
    metaVariableCtx <- getMetaVariableCtx @builtin
    let hasDepedendencies metaInfo = isNothing (metaSolution metaInfo) && not (null $ metaCtx metaInfo)
    let metasToRemove = MetaMap.filter hasDepedendencies metaVariableCtx
    orderedMetasToRemove <- orderMetasByTypeDependencies metasToRemove
    forM_ orderedMetasToRemove $ \(meta, metaInfo) -> do
      logCompilerSection2 MaxDetail ("removing dependencies of" <+> pretty meta) $ do
        void $ solveInTermsOfNewMetaWithDependencies meta metaInfo mempty

  logCompilerSection2 MaxDetail "removing dependencies from and merging instance constraints" $ do
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
  logCompilerSection2 MaxDetail "substituting metas through solution" $ do
    metaVariableCtx <- getMetaVariableCtx @builtin
    substMetaVariableCtx <- substMetaVariables metaVariableCtx
    modifyTypeCheckerDeclState (\s -> s {metaVariableCtx = substMetaVariableCtx})

  resultDecl <- substMetaVariables decl
  logUnsolvedUnknowns (Proxy @builtin)
  return resultDecl

orderMetasByTypeDependencies ::
  (MonadGeneralise builtin m) =>
  MetaMap (MetaInfo builtin) ->
  m [(MetaID, MetaInfo builtin)]
orderMetasByTypeDependencies metaCtx = do
  sortedMetas <- sortMetasByTypeDependencies metaCtx
  let lookupInfo meta = (meta, fromMaybe (developerError "Meta sorting gone wrong") $ MetaMap.lookup meta metaCtx)
  return $ reverse $ fmap lookupInfo sortedMetas

removeInstanceDependencies ::
  (MonadGeneralise builtin m) =>
  WithContext (InstanceConstraint builtin) ->
  m (WithContext (InstanceConstraint builtin))
removeInstanceDependencies c@(WithContext constraint ctx) =
  logCompilerSection MaxDetail "Removing dependencies:" $ do
    logDebug MaxDetail $ "Input: " <+> prettyExternal c
    let newCtx = updateConstraintBoundCtx ctx (const mempty)
    substConstraint <- substMetaVariablesAt (namedBoundCtxOf ctx) constraint
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
        result <- unify mempty (Forced mainGoal) (Forced $ getGoal otherConstraint)
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
  metaCtx <- metaVariableCtx <$> getTypeCheckerDeclState @builtin
  newMeta <- findUltimateUnsolvedMeta metaCtx originalMeta
  -- This is a hack that should disappear when we get records?
  updateMetaType @builtin newMeta (Quote.unnormalise 0 $ goalExpr $ instanceGoal constraint)
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
  sortedUnsolvedMetas <- sortMetasByTypeDependencies unsolvedMetas

  unsolvedInstanceConstraints <- getActiveInstanceConstraints
  unsolvedAuxInstanceConstraints <- getActiveAuxiliaryInstanceConstraints
  setInstanceConstraints @builtin mempty
  setAuxiliaryInstanceConstraints @builtin mempty

  let unsolvedConstraints = unsolvedInstanceConstraints <> unsolvedAuxInstanceConstraints
  let unsolvedConstraintMetas = MetaMap.fromList $ fmap ((\c -> (instanceSolution c, c)) . objectIn) unsolvedConstraints

  let p = provenanceOf decl
  binders <- traverse (createBinderForMeta unsolvedConstraintMetas p) (zip [1 ..] sortedUnsolvedMetas)
  generalisedDecl <- logCompilerSection2 MaxDetail ("generalisation over" <+> pretty sortedUnsolvedMetas) $ do
    foldlM prependBinderAndSolve decl binders
  logUnsolvedUnknowns (Proxy @builtin)
  return generalisedDecl

sortMetasByTypeDependencies ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  MetaVariableContext builtin ->
  m [MetaID]
sortMetasByTypeDependencies unsolvedMetas = do
  logCompilerSection2 MaxDetail "sorting generalisable constraints" $ do
    adjacencyMap <- traverse (metasIn (Proxy @builtin) . metaType) unsolvedMetas
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
  MetaInfo {..} <- getMetaInfo meta
  substMetaType <- substMetaVariablesAt (toNamedBoundCtx metaCtx) metaType
  let visibility = case MetaMap.lookup meta constraints of
        Just {} -> Instance True
        Nothing -> Implicit True

  -- Prepend the implicit binders for the new generalised variable.
  let binderName = "_t" <> Text.pack (show index)
  let binderDisplayForm = BinderDisplayForm (NameAndType binderName p) True
  let binder = Binder binderDisplayForm visibility metaRelevance substMetaType
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
  logCompilerSection2 MaxDetail ("generalising" <+> pretty meta <+> ":" <+> prettyVerbose binder) $ do
    let p = provenanceOf decl

    -- Create a new meta with dependencies on the telescope and solve the previous one in terms of it.
    metaInfo <- getMetaInfo meta
    let solutionCtx = binder : metaCtx metaInfo
    newMeta <- solveInTermsOfNewMetaWithDependencies meta metaInfo solutionCtx
    let solution = BoundVar p 0
    solveMeta newMeta solution solutionCtx

    -- Substitute the solution through the declaration (have to do this before prepending binders)
    substDecl <- substMetaVariables decl

    -- Compute the telescopes
    let typeBinder = binder
    let bodyBinder = mapBinderNamingForm (\t -> OnlyName (fromMaybe "_" (nameOf t)) p) binder

    -- Then finally update the declaration
    let alterType t = return $ Pi p typeBinder t
    let alterBody e = return $ Lam p bodyBinder e
    finalDecl <- case substDecl of
      DefFunction _ i s t e -> DefFunction p i (incrLHSBinderCount s) <$> alterType t <*> alterBody e
      DefAbstract _ i s t -> DefAbstract p i s <$> alterType t
      _ ->
        developerError $
          "Unsupported definition type in generalistion:"
            <> lineIndent (prettyVerbose substDecl)

    -- Substitute the new meta solution through.
    setCurrentDecl $ Just (finalDecl, False)

    logDebug MaxDetail $ "Result:" <+> lineIndent (prettyExternal finalDecl)
    return finalDecl

solveInTermsOfNewMetaWithDependencies ::
  (MonadGeneralise builtin m) =>
  MetaID ->
  MetaInfo builtin ->
  BoundCtx (Type builtin) ->
  m MetaID
solveInTermsOfNewMetaWithDependencies meta (MetaInfo p typ relevance oldCtx _) newCtx = do
  substType <- substMetaVariablesAt (toNamedBoundCtx oldCtx) typ
  (newMeta, newMetaExpr) <- freshMeta p substType relevance newCtx
  solveMeta meta newMetaExpr newCtx
  return newMeta
