{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Core where

import Prelude hiding (pi)
import Data.Map (Map)

import Vehicle.Prelude
import Vehicle.Language.AST

--------------------------------------------------------------------------------
-- Context definitions

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type BoundCtx = [(DBBinding, CheckedExpr, Maybe CheckedExpr)]

instance IsBoundCtx BoundCtx where
  ctxNames = map (\(n, _, _) -> n)

-- | The declarations that are currently in scope, indexed into via their names.
-- The first component is the type, and the second one the expression (if not
-- a postulate-style declaration).
type DeclCtx = Map Identifier (CheckedExpr, Maybe CheckedExpr)

instance Pretty DeclCtx where
  pretty = pretty . show

-- | Combined context
data VariableCtx = VariableCtx
  { boundCtx :: BoundCtx
  , declCtx  :: DeclCtx
  }

emptyVariableCtx :: VariableCtx
emptyVariableCtx = VariableCtx mempty mempty