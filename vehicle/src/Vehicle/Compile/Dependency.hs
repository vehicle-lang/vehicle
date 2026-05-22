module Vehicle.Compile.Dependency
  ( AdjacencyGraph (..),
    emptyAdjacencyGraph,
    insertNode,
    insertEdge,
    topologicalSort,
    DependencyGraph,
    createDependencyGraph,
    pruneUnusedDeclarations,
    pruneUnusedDeclarationsKeeping,
    completelyUnusedDeclarations,
  )
where

import Control.Monad.Writer (MonadWriter (..), execWriter)
import Data.Foldable (traverse_)
import Data.Graph (Graph, Vertex, dfs, graphFromEdges, indegree, vertices)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set (difference, fromList, notMember, toList)
import Data.Tree qualified as Tree
import GHC.Arr ((!))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude

--------------------------------------------------------------------------------
-- Adjacency graph

newtype AdjacencyGraph value = AdjacencyGraph
  { adjList :: Map value (Set value)
  }

emptyAdjacencyGraph :: (Ord value) => AdjacencyGraph value
emptyAdjacencyGraph = AdjacencyGraph mempty

insertNode :: (Ord value) => value -> AdjacencyGraph value -> AdjacencyGraph value
insertNode node (AdjacencyGraph adjList) =
  AdjacencyGraph $
    Map.insertWith (<>) node [] adjList

insertEdge :: (Ord value) => (value, value) -> AdjacencyGraph value -> AdjacencyGraph value
insertEdge (origin, dest) (AdjacencyGraph adjList) =
  AdjacencyGraph $
    Map.insertWith (<>) origin [dest] adjList

-- | Nodes reachable from @key@, ordered so each appears after everything it
-- depends on (post-order DFS of the reachable subforest).
topologicalSort :: (Ord value, Pretty value) => value -> AdjacencyGraph value -> [value]
topologicalSort key adjGraph = do
  let DependencyGraph {..} = fromEdges adjGraph
  case dfs graph [vertexFromIdent key] of
    [tree] -> identFromVertex <$> postOrder tree
    _ -> developerError "unexpected result from Graph.dfs"
  where
    postOrder :: Tree.Tree a -> [a]
    postOrder (Tree.Node v ts) = concatMap postOrder ts ++ [v]

--------------------------------------------------------------------------------
-- Types

data DependencyGraph value = DependencyGraph
  { graph :: Graph,
    identFromVertex :: Vertex -> value,
    dependenciesFromVertex :: Vertex -> [value],
    vertexFromIdent :: value -> Vertex
  }

fromEdges :: (Ord value, Pretty value) => AdjacencyGraph value -> DependencyGraph value
fromEdges (AdjacencyGraph outEdges) = do
  let outEdges' = (\(ident, out) -> (ident, ident, Set.toList out)) <$> Map.toList outEdges

  let (graph, nodeFromVertex', vertexFromIdent') = graphFromEdges outEdges'

  let identFromVertex v = (\(ident, _ident, _out) -> ident) $ nodeFromVertex' v
  let dependenciesFromVertex v = (\(_ident, _ident', out) -> out) $ nodeFromVertex' v
  let vertexFromIdent v = case vertexFromIdent' v of
        Just vertex -> vertex
        Nothing -> developerError $ "Missing requested node" <+> quotePretty v

  DependencyGraph
    { graph = graph,
      identFromVertex = identFromVertex,
      dependenciesFromVertex = dependenciesFromVertex,
      vertexFromIdent = vertexFromIdent
    }

--------------------------------------------------------------------------------
-- Constructing the dependency graph

createDependencyGraph :: [Decl builtin] -> DependencyGraph Identifier
createDependencyGraph ds = fromEdges $ AdjacencyGraph $ Map.fromList $ fmap goDecl ds
  where
    goDecl :: Decl builtin -> (Identifier, Set Identifier)
    goDecl d = (identifierOf d, execWriter (traverse_ go d))

    go :: (MonadWriter (Set Identifier) m) => Expr builtin -> m ()
    go = \case
      BoundVar {} -> return ()
      Universe {} -> return ()
      Meta {} -> return ()
      Hole {} -> return ()
      Builtin {} -> return ()
      FreeVar _ v -> do
        tell [v]
        return ()
      App fun args -> do go fun; traverse_ (traverse_ go) args
      Pi _ binder res -> do traverse_ go binder; go res
      Lam _ binder body -> do traverse_ go binder; go body
      Let _ bound binder body -> do go bound; traverse_ go binder; go body
      Record _ _ fields -> traverse_ (go . snd) fields
      RecordProj _ recordType record _ -> do go recordType; go record

--------------------------------------------------------------------------------
-- Completely unused declarations

completelyUnusedDeclarations :: [Decl builtin] -> Set Identifier
completelyUnusedDeclarations decls = do
  let DependencyGraph {..} = createDependencyGraph decls
  let indegrees = indegree graph
  let unusedVertices = filter (\v -> indegrees ! v == 0) (vertices graph)
  Set.fromList $ fmap identFromVertex unusedVertices

--------------------------------------------------------------------------------
-- Pruning

pruneUnusedDeclarations ::
  (MonadCompile m) =>
  Prog builtin ->
  m (Prog builtin)
pruneUnusedDeclarations = pruneUnusedDeclarationsKeeping mempty

-- | Like 'pruneUnusedDeclarations' but with an extra always-kept set, for
-- stdlib decls referenced from a builtin's type signature rather than from
-- the AST (see `typeBuiltinTypeLevelDeps`).
pruneUnusedDeclarationsKeeping ::
  (MonadCompile m) =>
  Set Identifier ->
  Prog builtin ->
  m (Prog builtin)
pruneUnusedDeclarationsKeeping extraRoots prog@(Main decls) = do
  logCompilerSection2 MinDetail "pruning unused declarations" $ do
    -- Prune all standard-library declarations that aren't used.
    let declsToCompile = mapMaybe (\d -> if isUserCode d then Just (nameOf d) else Nothing) decls
    if null declsToCompile
      then return prog
      else do
        let dependencyGraph = createDependencyGraph decls

        let userVertices = flip fmap declsToCompile $ \name -> do
              let ident = Identifier userModulePath name
              vertexFromIdent dependencyGraph ident
        let extraVertices =
              fmap (vertexFromIdent dependencyGraph) (Set.toList extraRoots)
        let startingVertices = userVertices <> extraVertices

        let declsToPrune = notReachableFrom dependencyGraph startingVertices
        logDebug MaxDetail $ "Pruning:" <+> indent 2 (prettySet pretty declsToPrune)

        return $ pruneProg prog declsToPrune

pruneProg :: Prog builtin -> Set Identifier -> Prog builtin
pruneProg (Main ds) declsToPrune = Main $ filter keepDecl ds
  where
    keepDecl :: Decl expr -> Bool
    keepDecl d = identifierOf d `Set.notMember` declsToPrune

notReachableFrom :: DependencyGraph Identifier -> [Vertex] -> Set Identifier
notReachableFrom DependencyGraph {..} origin = do
  let forest = dfs graph origin
  let reachableIdents = Set.fromList $ concatMap (fmap identFromVertex . Tree.flatten) forest
  let allIdents = Set.fromList $ fmap identFromVertex (vertices graph)
  Set.difference allIdents reachableIdents
