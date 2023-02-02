{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.VariableContext where

import Data.Map qualified as Map
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta.Map (MetaMap)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Declaration context

type MetaSubstitution builtin = MetaMap (GluedExpr builtin)

type DeclSubstitution builtin = DeclCtx (NormExpr builtin)

type TypingDeclCtxEntry builtin = (DBType builtin, Maybe (GluedExpr builtin))

type TypingDeclCtx builtin = DeclCtx (TypingDeclCtxEntry builtin)

toNormalisationDeclContext :: TypingDeclCtx builtin -> DeclCtx (DBExpr builtin)
toNormalisationDeclContext = Map.mapMaybe (fmap unnormalised . snd)

toDeclCtxEntry :: TypedDecl builtin -> TypingDeclCtxEntry builtin
toDeclCtxEntry decl = do
  let declType = unnormalised $ glued (typeOf decl)
  let declBody = fmap glued (bodyOf decl)
  (declType, declBody)

addToDeclCtx :: TypedDecl builtin -> TypingDeclCtx builtin -> TypingDeclCtx builtin
addToDeclCtx decl = Map.insert (identifierOf decl) (toDeclCtxEntry decl)

--------------------------------------------------------------------------------
-- Bound variable context

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type TypingBoundCtxEntry builtin = (Maybe Name, DBType builtin, Maybe (DBExpr builtin))

mkTypingBoundCtxEntry :: DBBinder builtin -> TypingBoundCtxEntry builtin
mkTypingBoundCtxEntry binder = (nameOf binder, binderType binder, Nothing)

type TypingBoundCtx builtin = BoundCtx (TypingBoundCtxEntry builtin)

instance HasBoundCtx (TypingBoundCtx builtin) where
  boundContextOf = map (\(n, _, _) -> n)
