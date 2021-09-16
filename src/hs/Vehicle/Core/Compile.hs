{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Vehicle.Core.Compile
  ( CompileError(..)
  , compile
  ) where

import Control.DeepSeq (force)
import Control.Monad.Except (ExceptT, withExceptT)

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Compile.Scope (ScopeError(..), runScopeCheck)
import Vehicle.Core.Compile.Descope (runDescope)
import Vehicle.Core.Compile.Type (TypingError(..), runTypeChecking)

data CompileError
  = ScopeError ScopeError
  | TypeError TypingError

instance MeaningfulError CompileError where
  details (ScopeError e) = details e
  details (TypeError  e) = details e

compile :: InputProg -> ExceptT CompileError Logger OutputProg
compile tree0 = do
  tree1 <- withExceptT ScopeError (runScopeCheck $ force tree0)
  tree2 <- withExceptT TypeError  (runTypeChecking $ force tree1)
  return $ runDescope $ force tree2