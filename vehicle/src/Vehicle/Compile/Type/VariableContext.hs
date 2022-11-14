{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.VariableContext where

import Data.Map qualified as Map

import Vehicle.Compile.Prelude
import Vehicle.Compile.Normalise.NormExpr (GluedExpr (..), GluedDecl, NormExpr)

--------------------------------------------------------------------------------
-- Declaration context

type TypingDeclCtxEntry = (CheckedType, Maybe GluedExpr)

type TypingDeclCtx = DeclCtx TypingDeclCtxEntry

addToDeclCtx :: GluedDecl -> TypingDeclCtx -> TypingDeclCtx
addToDeclCtx decl = Map.insert (identifierOf decl) (unnormalised (typeOf decl), bodyOf decl)

toNormalisationDeclContext :: TypingDeclCtx -> DeclCtx CheckedExpr
toNormalisationDeclContext = Map.mapMaybe (fmap unnormalised . snd)

toNBEDeclContext :: TypingDeclCtx -> DeclCtx NormExpr
toNBEDeclContext = Map.mapMaybe (fmap normalised . snd)

--------------------------------------------------------------------------------
-- Bound variable context

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type TypingBoundCtxEntry = (DBBinding, CheckedType, Maybe CheckedExpr)

type TypingBoundCtx = BoundCtx TypingBoundCtxEntry

instance HasBoundCtx TypingBoundCtx where
  boundContextOf = map (\(n, _, _) -> n)
