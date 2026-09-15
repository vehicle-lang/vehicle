module Vehicle.Compile.Dependency
  ( AdjacencyGraph (..),
    emptyAdjacencyGraph,
    insertNode,
    insertEdge,
    topologicalSort,
    DependencyGraph,
    createDependencyGraph,
    createAdjacencyGraph,
    pruneUnusedDeclarations,
    completelyUnusedDeclarations,
  )
where

import Control.Monad.Writer.Strict (MonadWriter (..), execWriter)
import Data.Foldable (traverse_)
import Data.Graph (Graph, Vertex, dfs, graphFromEdges, indegree, vertices)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set (difference, fromList, notMember, toList)
import Data.Tree qualified as Tree
import GHC.Arr ((!))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard

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

topologicalSort :: (Ord value, Pretty value) => value -> AdjacencyGraph value -> [value]
topologicalSort key adjGraph = do
  let DependencyGraph {..} = fromEdges adjGraph
  case dfs graph [vertexFromIdent key] of
    [e] -> reverse (identFromVertex <$> Tree.flatten e)
    _ -> developerError "unexpected result from Graph.dfs"

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

createDependencyGraph :: [Decl Builtin] -> DependencyGraph Identifier
createDependencyGraph ds = fromEdges $ createAdjacencyGraph ds

createAdjacencyGraph :: [Decl Builtin] -> AdjacencyGraph Identifier
createAdjacencyGraph ds = AdjacencyGraph $ Map.fromList $ fmap goDecl ds
  where
    goDecl :: Decl Builtin -> (Identifier, Set Identifier)
    goDecl d = (identifierOf d, execWriter (traverse_ go d))

    go :: (MonadWriter (Set Identifier) m) => Expr Builtin -> m ()
    go = \case
      BoundVar {} -> return ()
      Universe {} -> return ()
      Meta {} -> return ()
      Hole {} -> return ()
      Builtin _ b -> case b of
        DerivedFunction f -> do
          tell [identifierOf f]
        _ -> return ()
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

completelyUnusedDeclarations :: [Decl Builtin] -> Set Identifier
completelyUnusedDeclarations decls = do
  let DependencyGraph {..} = createDependencyGraph decls
  let indegrees = indegree graph
  let unusedVertices = filter (\v -> indegrees ! v == 0) (vertices graph)
  Set.fromList $ fmap identFromVertex unusedVertices

--------------------------------------------------------------------------------
-- Pruning

pruneUnusedDeclarations ::
  (MonadCompile m) =>
  (Decl Builtin -> Bool) ->
  Prog Builtin ->
  m (Prog Builtin)
pruneUnusedDeclarations isRootDecl prog@(Main decls) = do
  logCompilerSection2 MinDetail "pruning unused declarations" $ do
    -- Prune all standard-library declarations that aren't used.
    let declsToCompile = filter isRootDecl decls
    if null declsToCompile
      then return prog
      else do
        let dependencyGraph = createDependencyGraph decls
        let startingVertices = fmap identifierOf declsToCompile
        declsToPrune <- notReachableFrom dependencyGraph startingVertices
        logDebug MidDetail $ "Pruning:" <+> lineIndent (prettySet pretty declsToPrune)
        return $ pruneProg prog declsToPrune

pruneProg :: Prog Builtin -> Set Identifier -> Prog Builtin
pruneProg (Main ds) declsToPrune = Main $ filter keepDecl ds
  where
    keepDecl :: Decl expr -> Bool
    keepDecl d = identifierOf d `Set.notMember` declsToPrune

notReachableFrom :: (MonadLogger m) => DependencyGraph Identifier -> [Identifier] -> m (Set Identifier)
notReachableFrom DependencyGraph {..} origin = do
  let forest = dfs graph $ fmap vertexFromIdent origin
  let reachableIdents = Set.fromList $ concatMap (fmap identFromVertex . Tree.flatten) forest
  let allIdents = Set.fromList $ fmap identFromVertex (vertices graph)
  return $ Set.difference allIdents reachableIdents
