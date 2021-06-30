{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Vehicle.Core.Compile
  ( CompileError(..)
  , compile
  ) where

import Control.Arrow (right)
import Control.Monad.Supply (SupplyT, withSupplyT, runSupplyT)
import Control.Monad.Except (Except, withExcept, runExcept, mapExceptT, mapExcept)
import Control.Monad.Identity (Identity)
import Data.Text (pack)

import Vehicle.Core.AST(Tree, InputTree, Info, OutputTree, DeBruijn)
import Vehicle.Core.Compile.Scope (ScopeError(..), symbolToDeBruijn, deBruijnToSymbol)
import Vehicle.Core.Compile.Type (TypeError(..), checkInfer)
import Vehicle.Core.Compile.Dataflow
import Vehicle.Prelude
import Vehicle.Error ( MeaningfulError(..) )

data CompileError
  = ScopeError ScopeError
  | TypeError TypeError

instance MeaningfulError CompileError where
  details (ScopeError e) = details e
  details (TypeError  e) = details e

runSymbolToDeBruijn :: KnownSort sort =>
                       Tree (K Symbol) (K Provenance) sort ->
                       Except ScopeError (Tree DeBruijn (K Provenance) sort)
runSymbolToDeBruijn = symbolToDeBruijn

runTypeChecking :: KnownSort sort =>
                   Tree DeBruijn (K Provenance) sort ->
                   Except TypeError (Tree DeBruijn (Info DeBruijn :*: K Provenance) sort)
runTypeChecking = discardInferred . supplyMetaVars . propagateCtx . inferAndAnnotate
  where
    inferAndAnnotate = snd . checkInfer
    supplyMetaVars w = runSupplyT w (+ 1) (0 :: Integer)
    propagateCtx     = evalDataflowT mempty
    discardInferred  = mapExcept (right fst)

runDeBruijnToSymbol :: KnownSort sort =>
                       Tree DeBruijn (Info DeBruijn :*: K Provenance) sort ->
                       Except ScopeError (Tree (K Symbol) (Info (K Symbol) :*: K Provenance) sort)
runDeBruijnToSymbol = mapExceptT assignMachineVariables . deBruijnToSymbol
  where
    -- Machine variables are assigned the names: v1, v2, ...
    assignMachineVariables :: SupplyT Symbol Identity  (Either ScopeError (Tree (K Symbol) (Info (K Symbol) :*: K Provenance) sort))
                           -> Identity (Either ScopeError (OutputTree sort))
    assignMachineVariables s = runSupplyT (withSupplyT (\i -> "v" <> pack (show i)) s) (+ 1) (0 :: Integer)

compile ::
  (KnownSort sort) =>
  InputTree sort ->
  Either CompileError (OutputTree sort)
compile tree0 = runExcept $ do
  tree1 <- withExcept ScopeError (runSymbolToDeBruijn tree0)
  tree2 <- withExcept TypeError  (runTypeChecking tree1)
  tree3 <- withExcept ScopeError (runDeBruijnToSymbol tree2)
  return tree3