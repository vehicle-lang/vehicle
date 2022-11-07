
module Vehicle.Compile.DependencyAnalysis
  ( analyseDependenciesAndPrune
  ) where

import Control.Monad (forM_)
import Data.Set (Set)
import Data.Set qualified as Set (difference, fromList, map, member)

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude

analyseDependenciesAndPrune :: MonadCompile m
                            => UncheckedProg
                            -> UncheckedPropertyContext
                            -> DependencyGraph
                            -> DeclarationNames
                            -> m UncheckedProg
analyseDependenciesAndPrune prog propertyContext dependencyGraph declarationsToCompile = do
  checkForDeadCode prog propertyContext dependencyGraph

  if null declarationsToCompile
    then return prog
    else do
      let declsToKeep = reachableFrom dependencyGraph (Set.map Identifier declarationsToCompile)
      return $ pruneProg prog declsToKeep

checkForDeadCode :: MonadCompile m => UncheckedProg -> UncheckedPropertyContext -> DependencyGraph -> m ()
checkForDeadCode prog properties dependencyGraph = do
  let reachableDecls = reachableFrom dependencyGraph properties
  let allDecls = allDeclsIn prog
  let unreachableDecls = Set.difference allDecls reachableDecls
  forM_ unreachableDecls $ \_d ->
    return () -- logWarning $ "unused declaration" <+> quotePretty d <+> ""

pruneProg :: UncheckedProg -> Set Identifier -> UncheckedProg
pruneProg (Main ds) declsToKeep = Main $ filter keepDecl ds
  where
    keepDecl :: UncheckedDecl -> Bool
    keepDecl d = identifierOf d `Set.member` declsToKeep

allDeclsIn :: UncheckedProg -> Set Identifier
allDeclsIn (Main ds) = Set.fromList $ fmap identifierOf ds
