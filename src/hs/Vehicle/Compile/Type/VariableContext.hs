{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.VariableContext where

import Control.Monad.Reader
import Data.Map qualified as Map

import Vehicle.Compile.Prelude

--------------------------------------------------------------------------------
-- Declaration context

type TypingDeclCtxEntry = (CheckedExpr, Maybe CheckedExpr)

type TypingDeclCtx = DeclCtx TypingDeclCtxEntry

--------------------------------------------------------------------------------
-- Bound variable context

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type TypingBoundCtxEntry = (DBBinding, CheckedExpr, Maybe CheckedExpr)

type TypingBoundCtx = BoundCtx TypingBoundCtxEntry

instance HasBoundCtx TypingBoundCtx where
  boundContextOf = map (\(n, _, _) -> n)

--------------------------------------------------------------------------------
-- Variable context

type TypingVariableCtx = VariableCtx TypingDeclCtxEntry TypingBoundCtxEntry

addDeclToCtx :: MonadReader TypingVariableCtx m => CheckedDecl -> m a -> m a
addDeclToCtx decl = addToDeclCtx (identifierOf decl) (typeOf decl, bodyOf decl)

instance HasBoundCtx TypingVariableCtx where
  boundContextOf = boundContextOf . boundCtx

getNormalisationContext :: MonadReader TypingVariableCtx m => m (DeclCtx CheckedExpr)
getNormalisationContext = Map.mapMaybe snd <$> getDeclCtx