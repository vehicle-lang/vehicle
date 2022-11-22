
module Vehicle.Compile.Dependency.Graph
  ( DependencyGraph
  , Dependencies
  , DependencyList
  , fromEdges
  , reachableFrom
  ) where

import Data.Graph
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree qualified as Tree

import Vehicle.Prelude
import Vehicle.Syntax.AST

type Dependencies = [Identifier]

type DependencyList = [(Identifier, Dependencies)]

data DependencyGraph = DependencyGraph
  { graph                  :: Graph
  , identFromVertex        :: Vertex -> Identifier
  , dependenciesFromVertex :: Vertex -> Dependencies
  , vertexFromIdent        :: Identifier -> Vertex
  }

fromEdges :: [(Identifier, [Identifier])] -> DependencyGraph
fromEdges outEdges = do
  let outEdges' = fmap (\(ident, out) -> (ident, ident, out)) outEdges

  let (graph, nodeFromVertex', identFromVertex') = graphFromEdges outEdges'

  let identFromVertex v = (\(ident, _ident, _out) -> ident) $ nodeFromVertex' v
  let dependenciesFromVertex v = (\(_ident, _ident', out) -> out) $ nodeFromVertex' v
  let vertexFromIdent i = fromMaybe (missingNodeError i) (identFromVertex' i)

  DependencyGraph
    { graph                  = graph
    , identFromVertex        = identFromVertex
    , dependenciesFromVertex = dependenciesFromVertex
    , vertexFromIdent        = vertexFromIdent
    }

reachableFrom :: DependencyGraph -> Set Identifier -> Set Identifier
reachableFrom DependencyGraph{..} nodes = do
  let forest = dfs graph (fmap vertexFromIdent (Set.toList nodes))
  Set.fromList $ concatMap (fmap identFromVertex . Tree.flatten) forest

missingNodeError :: Identifier -> a
missingNodeError i = developerError $ "Declaration" <+> pretty i <+> "missing in dependency graph"
