{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.VariableContext where

import Data.Map qualified as Map

import Vehicle.Compile.Prelude
import Control.Monad.Reader

--------------------------------------------------------------------------------
-- Bound variable context

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type TypingBoundCtx = [(DBBinding, CheckedExpr, Maybe CheckedExpr)]

instance HasBoundCtx TypingBoundCtx where
  boundContextOf = map (\(n, _, _) -> n)

--------------------------------------------------------------------------------
-- Declaration context

type TypingDeclCtx = DeclCtx (CheckedExpr, Maybe CheckedExpr)

--------------------------------------------------------------------------------
-- Shared context

-- | Combined context
data VariableCtx = VariableCtx
  { boundCtx :: TypingBoundCtx
  , declCtx  :: TypingDeclCtx
  } deriving (Show)

instance HasBoundCtx VariableCtx where
  boundContextOf (VariableCtx boundCtx _) = boundContextOf boundCtx

emptyVariableCtx :: VariableCtx
emptyVariableCtx = VariableCtx mempty mempty

getDeclCtx :: MonadReader VariableCtx m => m TypingDeclCtx
getDeclCtx = asks declCtx

addToDeclCtx :: MonadReader VariableCtx m => CheckedDecl -> m a -> m a
addToDeclCtx decl = local addDecl
  where
    declName = identifierOf decl
    declType = typeOf decl
    declBody = bodyOf decl

    addDecl :: VariableCtx -> VariableCtx
    addDecl VariableCtx{..} = VariableCtx
      { declCtx = Map.insert declName (declType, declBody) declCtx
      , ..
      }

getBoundCtx :: MonadReader VariableCtx m => m TypingBoundCtx
getBoundCtx = asks boundCtx

getVariableCtx :: MonadReader VariableCtx m => m VariableCtx
getVariableCtx = ask

addToBoundCtx :: MonadReader VariableCtx m
              => DBBinding
              -> CheckedExpr
              -> Maybe CheckedExpr
              -> m a
              -> m a
addToBoundCtx n t e = local add
  where
    add :: VariableCtx -> VariableCtx
    add VariableCtx{..} = VariableCtx{ boundCtx = (n, t, e) : boundCtx, ..}
