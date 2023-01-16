module Vehicle.Compile.Dependency.Analysis
  ( analyseDependenciesAndPrune,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Data.Set (Set)
import Data.Set qualified as Set (member, toList)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude

analyseDependenciesAndPrune ::
  MonadCompile m =>
  GenericProg expr ->
  DependencyGraph ->
  DeclarationNames ->
  m (GenericProg expr)
analyseDependenciesAndPrune prog dependencyGraph declarationsToCompile = do
  if null declarationsToCompile
    then return prog
    else do
      startingVertices <- forM (Set.toList declarationsToCompile) $ \name ->
        case vertexFromIdent dependencyGraph (Identifier User name) of
          Just vertex -> return vertex
          Nothing -> throwError $ InvalidPrunedName name

      let declsToKeep = reachableFrom dependencyGraph startingVertices
      return $ pruneProg prog declsToKeep

pruneProg :: GenericProg expr -> Set Identifier -> GenericProg expr
pruneProg (Main ds) declsToKeep = Main $ filter keepDecl ds
  where
    keepDecl :: GenericDecl expr -> Bool
    keepDecl d = identifierOf d `Set.member` declsToKeep
