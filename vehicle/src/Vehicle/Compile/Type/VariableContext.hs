{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.VariableContext where

import Data.Map qualified as Map

import Vehicle.Compile.Prelude

--------------------------------------------------------------------------------
-- Declaration context

type TypingDeclCtxEntry = (CheckedType, Maybe CheckedExpr)

type TypingDeclCtx = DeclCtx TypingDeclCtxEntry

addToDeclCtx :: CheckedDecl -> TypingDeclCtx -> TypingDeclCtx
addToDeclCtx decl = Map.insert (identifierOf decl) (typeOf decl, bodyOf decl)

toNormalisationDeclContext :: TypingDeclCtx -> DeclCtx CheckedExpr
toNormalisationDeclContext = Map.mapMaybe snd

--------------------------------------------------------------------------------
-- Bound variable context

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type TypingBoundCtxEntry = (DBBinding, CheckedType, Maybe CheckedExpr)

type TypingBoundCtx = BoundCtx TypingBoundCtxEntry

instance HasBoundCtx TypingBoundCtx where
  boundContextOf = map (\(n, _, _) -> n)
