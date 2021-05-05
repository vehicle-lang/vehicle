{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vehicle.Core.DeBruijn.Substitution
  ( subst
  ) where

import Vehicle.Core.Type
import Vehicle.Core.DeBruijn.Core ( SortedDeBruijn(..), DeBruijnIndex(..) )

-- Implementation of substitution and lifting for De Bruijn indexed terms.
-- Code loosely based off of http://blog.discus-lang.org/2011/08/how-i-learned-to-stop-worrying-and-love.html


-- * DeBruijn lifting

class DeBruijnLifting t where
  -- | Lift all deBruin indices that refer to environment variables by 1.
  lift :: Int -- ^ current binding depth
        -> t  -- ^ expression containing the variable references to lift
        -> t  -- ^ the result of the lifting

instance DeBruijnLifting (Type SortedDeBruijn builtin ann) where
  lift _ expr@(TCon _ _) = expr
  lift _ expr@(TLitDim _ _) = expr
  lift _ expr@(TMeta _ _) = expr

  lift d (TApp ann fn arg) = TApp ann (lift d fn) (lift d arg)
  lift d (TLitList ann typs) = TLitList ann (map (lift d) typs)

  lift d (TForall ann arg body) = TForall ann arg
    -- Increase the depth as we move across a binding site
    (lift (d + 1) body)

  lift d (TVar ann (SortedDeBruijn (DeBruijnIndex (pos, i)))) = TVar ann (SortedDeBruijn (DeBruijnIndex (pos,
    if d <= i
      -- Index is referencing the environment so increment it
      then i + 1
      -- Index is locally bound so no need to increment it
      else i)))

instance DeBruijnLifting (Expr SortedDeBruijn builtin ann) where
  lift _ expr@(ELitInt _ _) = expr
  lift _ expr@(ELitReal _ _) = expr
  lift _ expr@(ECon _ _) = expr

  lift d (EApp ann exp1 exp2) = EApp ann (lift d exp1) (lift d exp2)
  lift d (ELitSeq ann exprs) = ELitSeq ann (map (lift d) exprs)
  lift d (EAnn ann expr typ) = EAnn ann (lift d expr) typ
  lift d (ETyLam ann targ expr) = ETyLam ann targ (lift d expr)
  lift d (ETyApp ann expr typ) = ETyApp ann (lift d expr) typ

  lift d (EVar ann (SortedDeBruijn (DeBruijnIndex (pos, i)))) = EVar ann (SortedDeBruijn (DeBruijnIndex (pos,
    if d <= i
      -- Index is referencing the environment so increment it
      then i + 1
      -- Index is locally bound so no need to increment it
      else i)))

  lift d (ELet ann arg exp1 exp2) = ELet ann arg
    -- Maintain the current depth as move across the let bound expression
    (lift d exp1)
    -- Increase the current depth as we move across the variable
    (lift (d + 1) exp2)

  lift d (ELam ann arg expr) = ELam ann arg
    -- Increase the current depth as we move across a lambda.
    (lift (d + 1) expr)



-- * DeBruijn substitution

class DeBruijnSubstitution t where
  subst :: Int -- ^ current binding depth
        -> t   -- ^ expression to substitute
        -> t   -- ^ expression to substitute into
        -> t   -- ^ the result of the substitution

instance DeBruijnSubstitution (Type SortedDeBruijn builtin ann) where
  subst _ _ expr@(TCon _ _) = expr
  subst _ _ expr@(TLitDim _ _) = expr
  subst _ _ expr@(TMeta _ _) = expr

  subst d sub (TApp ann fn arg) = TApp ann (subst d sub fn) (subst d sub arg)
  subst d sub (TLitList ann typs) = TLitList ann (map (subst d sub) typs)

  subst d sub (TForall ann arg body) =
    -- Increase the depth as we move across a binding site
    TForall ann arg (subst (d + 1) (lift 0 sub) body)

  subst d sub (TVar ann (SortedDeBruijn (DeBruijnIndex (pos, i)))) =
    case compare i d of
      -- Index matches the expression we're substituting for
      EQ -> sub
      -- Index was bound in the original type
      LT -> TVar ann (SortedDeBruijn (DeBruijnIndex (pos, i)))
      -- Index was free in the original type, and we've removed a binder so decrease it by 1.
      GT -> TVar ann (SortedDeBruijn (DeBruijnIndex (pos, i - 1)))

instance DeBruijnSubstitution (Expr SortedDeBruijn builtin ann) where
  subst _ _ expr@(ELitInt _ _) = expr
  subst _ _ expr@(ELitReal _ _) = expr
  subst _ _ expr@(ECon _ _) = expr

  subst d sub (ELitSeq ann exprs) = ELitSeq ann (map (subst d sub) exprs)
  subst d sub (EAnn ann expr typ) = EAnn ann (subst d sub expr) typ
  subst d sub (ETyLam ann targ expr) = ETyLam ann targ (subst d sub expr)
  subst d sub (ETyApp ann expr typ) = ETyApp ann (subst d sub expr) typ
  subst d sub (EApp ann exp1 exp2) = EApp ann (subst d sub exp1) (subst d sub exp2)

  subst d sub (EVar ann (SortedDeBruijn (DeBruijnIndex (pos , i)))) =
    case compare i d of
      -- Index matches the expression we're substituting for
      EQ -> sub
      -- Index was bound in the original expression
      LT -> EVar ann (SortedDeBruijn (DeBruijnIndex (pos, i)))
      -- Index was free in the original expression, and we've removed a binder so decrease it by 1.
      GT -> EVar ann (SortedDeBruijn (DeBruijnIndex (pos, i - 1)))

  subst d sub (ELet ann arg exp1 exp2) =
    -- Increase the depth as we move across a binding site
    ELet ann arg (subst d sub exp1) (subst (d + 1) (lift 0 sub) exp2)

  subst d sub (ELam ann arg expr) =
    -- Increase the depth as we move across a binding site
    ELam ann arg (subst (d + 1) (lift 0 sub) expr)
