{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.VariableContext where

import Data.Map qualified as Map
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta.Map (MetaMap)
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Declaration context

type MetaSubstitution = MetaMap GluedExpr

type DeclSubstitution = DeclCtx NormExpr

type TypingDeclCtxEntry = (CheckedType, Maybe GluedExpr)

type TypingDeclCtx = DeclCtx TypingDeclCtxEntry

toNormalisationDeclContext :: TypingDeclCtx -> DeclCtx CheckedExpr
toNormalisationDeclContext = Map.mapMaybe (fmap unnormalised . snd)

toDeclCtxEntry :: TypedDecl -> TypingDeclCtxEntry
toDeclCtxEntry decl = do
  let declType = unnormalised $ glued (typeOf decl)
  let declBody = fmap glued (bodyOf decl)
  (declType, declBody)

addToDeclCtx :: TypedDecl -> TypingDeclCtx -> TypingDeclCtx
addToDeclCtx decl = Map.insert (identifierOf decl) (toDeclCtxEntry decl)

--------------------------------------------------------------------------------
-- Bound variable context

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type TypingBoundCtxEntry = (Maybe Name, CheckedType, Maybe CheckedExpr)

mkTypingBoundCtxEntry :: CheckedBinder -> TypingBoundCtxEntry
mkTypingBoundCtxEntry binder = (nameOf binder, binderType binder, Nothing)

type TypingBoundCtx = BoundCtx TypingBoundCtxEntry

instance HasBoundCtx TypingBoundCtx where
  boundContextOf = map (\(n, _, _) -> n)
