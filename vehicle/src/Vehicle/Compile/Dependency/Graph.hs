module Vehicle.Compile.Dependency.Graph
  ( DependencyGraph (vertexFromIdent),
    Dependencies,
    DependencyList,
    fromEdges,
    reachableFrom,
  )
where

import Data.Graph
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree qualified as Tree
import Vehicle.Syntax.AST

type Dependencies = [Identifier]

type DependencyList = [(Identifier, Dependencies)]

data DependencyGraph = DependencyGraph
  { graph :: Graph,
    identFromVertex :: Vertex -> Identifier,
    dependenciesFromVertex :: Vertex -> Dependencies,
    vertexFromIdent :: Identifier -> Maybe Vertex
  }

fromEdges :: [(Identifier, [Identifier])] -> DependencyGraph
fromEdges outEdges = do
  let outEdges' = fmap (\(ident, out) -> (ident, ident, out)) outEdges

  let (graph, nodeFromVertex', vertexFromIdent) = graphFromEdges outEdges'

  let identFromVertex v = (\(ident, _ident, _out) -> ident) $ nodeFromVertex' v
  let dependenciesFromVertex v = (\(_ident, _ident', out) -> out) $ nodeFromVertex' v

  DependencyGraph
    { graph = graph,
      identFromVertex = identFromVertex,
      dependenciesFromVertex = dependenciesFromVertex,
      vertexFromIdent = vertexFromIdent
    }

reachableFrom :: DependencyGraph -> [Vertex] -> Set Identifier
reachableFrom DependencyGraph {..} origin = do
  let forest = dfs graph origin
  Set.fromList $ concatMap (fmap identFromVertex . Tree.flatten) forest
