module Vehicle.Compile.Dependency
  ( analyseDependenciesAndPrune,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Writer (MonadWriter (..), execWriterT)
import Data.Foldable (traverse_)
import Data.Graph (Graph, Vertex, dfs, graphFromEdges)
import Data.Set (Set)
import Data.Set qualified as Set (fromList, member)
import Data.Tree qualified as Tree
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude

--------------------------------------------------------------------------------
-- Types

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

--------------------------------------------------------------------------------
-- Constructing the dependency graph

constructGraph ::
  forall m builtin.
  (MonadLogger m) =>
  Prog builtin ->
  m DependencyGraph
constructGraph prog = do
  depsList <- goProg prog
  return $ fromEdges depsList
  where
    goProg :: (MonadLogger m) => Prog builtin -> m DependencyList
    goProg (Main ds) = traverse goDecl ds

    goDecl :: (MonadLogger m) => Decl builtin -> m (Identifier, Dependencies)
    goDecl d = do
      deps <- execWriterT (traverse_ go d)
      return (identifierOf d, deps)

    go :: forall m1. (MonadLogger m1, MonadWriter [Identifier] m1) => Expr builtin -> m1 ()
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

analyseDependenciesAndPrune ::
  (MonadCompile m) =>
  Prog expr ->
  DeclarationNames ->
  m (Prog expr)
analyseDependenciesAndPrune prog declarationsToCompile = do
  if null declarationsToCompile
    then return prog
    else do
      dependencyGraph <- constructGraph prog
      startingVertices <- forM declarationsToCompile $ \name ->
        case vertexFromIdent dependencyGraph (Identifier (ModulePath [User]) name) of
          Just vertex -> return vertex
          Nothing -> throwError $ MissingPrunedName name

      let declsToKeep = reachableFrom dependencyGraph startingVertices
      return $ pruneProg prog declsToKeep

pruneProg :: GenericProg expr -> Set Identifier -> GenericProg expr
pruneProg (Main ds) declsToKeep = Main $ filter keepDecl ds
  where
    keepDecl :: GenericDecl expr -> Bool
    keepDecl d = identifierOf d `Set.member` declsToKeep

reachableFrom :: DependencyGraph -> [Vertex] -> Set Identifier
reachableFrom DependencyGraph {..} origin = do
  let forest = dfs graph origin
  Set.fromList $ concatMap (fmap identFromVertex . Tree.flatten) forest
