
module Vehicle.Core.Compile.Unify
  ( UnificationConstraint(..)
  , UnificationError(..)
  , TypeClassConstraint(..)
  ) where

import Data.IntMap (IntMap)
import Data.Sequence (Seq)

import Vehicle.Prelude
import Vehicle.Core.AST

data UnificationConstraint
  = Unify Provenance [(Name, Name)] CheckedExpr CheckedExpr

data TypeClassConstraint   = Meta `Has` CheckedExpr


newtype UnificationError = Failure UnificationConstraint

newtype UnificationSubst = UnificationSubst (IntMap CheckedExpr)
{-
instance Semigroup UnificationSubst

instance Monoid UnificationSubst where
  mempty = UnificationSubst mempty


decomposeApp :: CheckedExpr -> (CheckedExpr, [CheckedBinder])
decomposeApp = _

data UnificationResult
  = StuckOn  [Meta]
  | Progress UnificationSubst (Seq UnificationConstraint)

unify1Simple :: Eq a => UnificationConstraint -> a -> a -> Either UnificationError UnificationResult
unify1Simple c v1 v2
  | v1 == v2 = Right $ Progress mempty subst
  | otherwise  = Left $ Failure c

unify1 :: UnificationSubst -> UnificationConstraint ->
          Either UnificationError UnificationResult

-- Rigid-rigid
unify1 subst c@(Unify _ _ (Type l1) (Type l2)) = unify1Simple c l1 l2

unify1 subst c@(Unify _ _ Constraint Constraint) = Right $ Progress mempty subst

unify1 subst c@(Unify p _ (Ann _ e1 t1) (Ann _ e2 t2)) = Right $ Progress subst
  [ Unify p e1 e2
  , Unify p t1 t2
  ]

unify1 subst c@(Unify _ _ (Builtin _ op1) (Builtin _ op2)) = unify1Simple c op1 op2
unify1 subst c@(Unify _ _ (Var _ v1) (Var _ v2)) = unify1Simple c v1 v2

unify1 subst c@(Unify p ctx (Lam _ binder1 body1) (Lam _ binder2 body2)) = Right $ Progress subst
  [ Unify p _ body1 body2 ]

unify1 subst c@(Unify _ _ (Literal  _ l1) (Literal  _ l2)) = unify1Simple c l1 l2
unify1 subst c@(Unify p ctx (Seq      _ es1) (Seq      _ es2))
  | length es1 == length es2 = Right $ Progress subst (zip (Unify p ctx))
  -- TODO more informative error message
  | otherwise                = Left $ Failure c

-- TODO need to try and unify `Seq` with `Cons`s.

unify1 subst c@(Unify p ctx (Pi _ binder1 body1) (Pi _ binder2 body2)) =
  -- !!TODO!! Block until binders are solved
  Right $ Progress subst [Unify p ctx _ _,  Unify p _ body1 body2]

unify1 subst c@(Unify p ctx e1@App{} e2@App{}) =
  case (decomposeApp e1, decomposeApp e2) of
    ((Var _ i, e1Args), (Var _ j, e2Args))
      | i != j    -> Left  $ Failure c
      | otherwise -> Right $ Progress subst (zip (Unify p ctx e1Args e2Args))
{-
    ((Var _ i, ))
  -- If two heads equal variables then check that all args equal
  -- If two heads meta-variables then flex-flex
  -- If one each then flex-rigid
-}

-- Rigid-flex
unify1 subst (Unify p ctx e1 e2) = _

-- Flex-flex
unify1 subst c@(Unify _ _ (Meta _   _) (Meta _   _)) = _


-- Errors/catch-all

unify1 subst c@(Unify _ _ Let {} _) = developerError $ _
unify1 subst c@(Unify _ _ _ Let {}) = developerError $ _

unify1 subst c@(Unify _ _ (Hole _ _) _) = _
unify1 subst c@(Unify _ _ _ (Hole _ _)) = _




unify :: Seq UnificationConstraint -> Either UnificationError UnificationSubst
unify = _
-}