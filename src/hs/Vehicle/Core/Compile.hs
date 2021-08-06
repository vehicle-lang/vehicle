{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Vehicle.Core.Compile
  ( CompileError(..)
  , compile
  ) where

import Control.DeepSeq (force)
import Control.Monad.Except (withExcept, runExcept)

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Compile.Scope (ScopeError(..), scopeProg)
import Vehicle.Core.Compile.Descope (runDescope)
import Vehicle.Core.Compile.Type (TypingError(..), runTypeChecking)

data CompileError
  = ScopeError ScopeError
  | TypeError TypingError

instance MeaningfulError CompileError where
  details (ScopeError e) = details e
  details (TypeError  e) = details e

compile :: InputProg -> Either CompileError OutputProg
compile tree0 = runExcept $ do
  tree1 <- withExcept ScopeError (scopeProg $ force tree0)
  tree2 <- withExcept TypeError  (runTypeChecking $ force tree1)
  return $ runDescope $ force tree2