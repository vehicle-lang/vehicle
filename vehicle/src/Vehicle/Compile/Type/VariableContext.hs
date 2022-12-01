{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.VariableContext where

import Data.Map qualified as Map

import Vehicle.Compile.Prelude
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised
import Vehicle.Compile.Type.Output

--------------------------------------------------------------------------------
-- Declaration context

type TypingDeclCtxEntry = (CheckedType, Maybe GluedExpr)

type TypingDeclCtx = DeclCtx TypingDeclCtxEntry

toNormalisationDeclContext :: TypingDeclCtx -> DeclCtx CheckedExpr
toNormalisationDeclContext = Map.mapMaybe (fmap unnormalised . snd)

toNBEDeclContext :: TypingDeclCtx -> DeclCtx NormExpr
toNBEDeclContext = Map.mapMaybe (fmap normalised . snd)

addToDeclCtx :: TypedDecl -> TypingDeclCtx -> TypingDeclCtx
addToDeclCtx decl = do
  let declType = unnormalised $ glued (typeOf decl)
  let declBody = fmap glued (bodyOf decl)
  Map.insert (identifierOf decl) (declType, declBody)

--------------------------------------------------------------------------------
-- Bound variable context

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type TypingBoundCtxEntry = (DBBinding, CheckedType, Maybe CheckedExpr)

type TypingBoundCtx = BoundCtx TypingBoundCtxEntry

instance HasBoundCtx TypingBoundCtx where
  boundContextOf = map (\(n, _, _) -> n)
