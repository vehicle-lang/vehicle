module Vehicle.Compile.Dependency
  ( DependencyGraph,
    createDependencyGraph,
    pruneUnusedDeclarations,
    completelyUnusedDeclarations,
  )
where

import Control.Monad (forM)
import Control.Monad.Writer (MonadWriter (..), execWriter)
import Data.Foldable (traverse_)
import Data.Graph (Graph, Vertex, dfs, graphFromEdges, indegree, vertices)
import Data.Set (Set)
import Data.Set qualified as Set (difference, fromList, notMember)
import Data.Tree qualified as Tree
import GHC.Arr ((!))
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

--------------------------------------------------------------------------------
-- Constructing the dependency graph

createDependencyGraph :: Prog builtin -> DependencyGraph
createDependencyGraph prog = fromEdges (goProg prog)
  where
    goProg :: Prog builtin -> DependencyList
    goProg (Main ds) = fmap goDecl ds

    goDecl :: Decl builtin -> (Identifier, Dependencies)
    goDecl d = (identifierOf d, execWriter (traverse_ go d))

    go :: (MonadWriter [Identifier] m) => Expr builtin -> m ()
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
-- Completely unused declarations

completelyUnusedDeclarations :: DependencyGraph -> Set Identifier
completelyUnusedDeclarations DependencyGraph {..} = do
  let indegrees = indegree graph
  let unusedVertices = filter (\v -> indegrees ! v == 0) (vertices graph)
  Set.fromList $ fmap identFromVertex unusedVertices

--------------------------------------------------------------------------------
-- Pruning

pruneUnusedDeclarations ::
  (MonadCompile m) =>
  Prog expr ->
  DependencyGraph ->
  DeclarationNames ->
  m (Prog expr)
pruneUnusedDeclarations prog dependencyGraph declarationsToCompile
  | null declarationsToCompile = return prog
  | otherwise = do
      logCompilerPass MinDetail "Pruning unused declarations" $ do
        startingVertices <- forM declarationsToCompile $ \name ->
          case vertexFromIdent dependencyGraph (Identifier (ModulePath [User]) name) of
            Just vertex -> return vertex
            Nothing ->
              -- This should have been caught earlier when we first prune the declarations
              compilerDeveloperError $ "Missing requested declaration" <+> quotePretty name

        let declsToPrune = notReachableFrom dependencyGraph startingVertices
        logDebug MaxDetail $ "Pruning:" <+> indent 2 (prettySet declsToPrune)

        return $ pruneProg prog declsToPrune

pruneProg :: GenericProg expr -> Set Identifier -> GenericProg expr
pruneProg (Main ds) declsToPrune = Main $ filter keepDecl ds
  where
    keepDecl :: GenericDecl expr -> Bool
    keepDecl d = identifierOf d `Set.notMember` declsToPrune

notReachableFrom :: DependencyGraph -> [Vertex] -> Set Identifier
notReachableFrom DependencyGraph {..} origin = do
  let forest = dfs graph origin
  let reachableIdents = Set.fromList $ concatMap (fmap identFromVertex . Tree.flatten) forest
  let allIdents = Set.fromList $ fmap identFromVertex (vertices graph)
  Set.difference allIdents reachableIdents
