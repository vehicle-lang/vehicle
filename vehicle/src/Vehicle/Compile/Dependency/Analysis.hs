module Vehicle.Compile.Dependency.Analysis
  ( analyseDependenciesAndPrune,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set (map, member)
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
      let declsToCompile = Set.map (Identifier User) declarationsToCompile
      let declsToKeep = reachableFrom dependencyGraph declsToCompile
      return $ pruneProg prog declsToKeep

pruneProg :: GenericProg expr -> Set Identifier -> GenericProg expr
pruneProg (Main ds) declsToKeep = Main $ filter keepDecl ds
  where
    keepDecl :: GenericDecl expr -> Bool
    keepDecl d = identifierOf d `Set.member` declsToKeep
