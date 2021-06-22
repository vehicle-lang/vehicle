{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vehicle.Core.Normalise.DeBruijnSubstitution
  ( subst
  ) where

import Vehicle.Core.AST

-- * DeBruijn substitution

class DeBruijnSubstitution t where
  subst :: BindingDepth -- ^ current binding depth
        -> t            -- ^ expression to substitute
        -> t            -- ^ expression to substitute into
        -> t            -- ^ the result of the substitution

instance DeBruijnSubstitution (Type DeBruijn ann) where
  subst _ _ expr@(TCon _ _)    = expr
  subst _ _ expr@(TLitDim _ _) = expr
  subst _ _ expr@(TMeta _ _)   = expr

  subst d sub (TApp ann fn arg)      = TApp ann (subst d sub fn) (subst d sub arg)
  subst d sub (TLitDimList ann typs) = TLitDimList ann (map (subst d sub) typs)

  subst d sub (TForall ann arg body) =
    -- Increase the depth as we move across a binding site
    TForall ann arg (subst (incrTypeDepth d) (liftDeBruijn initialBindingDepth sub) body)

  subst d sub (TVar ann (TIndex i))  =
    case compare i (typeDepth d) of
      -- Index matches the expression we're substituting for
      EQ -> sub
      -- Index was bound in the original type
      LT -> TVar ann (TIndex i)
      -- Index was free in the original type, and we've removed a binder so decrease it by 1.
      GT -> TVar ann (TIndex (i - 1))

instance DeBruijnSubstitution (Expr DeBruijn ann) where
  subst _ _ expr@(ELitInt _ _)  = expr
  subst _ _ expr@(ELitReal _ _) = expr
  subst _ _ expr@(ECon _ _)     = expr

  subst d sub (ELitSeq ann exprs)     = ELitSeq ann (map (subst d sub) exprs)
  subst d sub (EAnn    ann expr typ)  = EAnn    ann (subst d sub expr) typ
  subst d sub (ETyLam  ann targ expr) = ETyLam  ann targ (subst d sub expr)
  subst d sub (ETyApp  ann expr typ)  = ETyApp  ann (subst d sub expr) typ
  subst d sub (EApp    ann exp1 exp2) = EApp    ann (subst d sub exp1) (subst d sub exp2)

  subst d sub (EVar ann (EIndex i)) =
    case compare i (exprDepth d) of
      -- Index matches the expression we're substituting for
      EQ -> sub
      -- Index was bound in the original expression
      LT -> EVar ann (EIndex i)
      -- Index was free in the original expression, and we've removed a binder so decrease it by 1.
      GT -> EVar ann (EIndex (i - 1))

  subst d sub (ELet ann arg exp1 exp2) =
    -- Increase the depth as we move across a binding site
    ELet ann arg (subst d sub exp1) (subst (incrExprDepth d) (liftDeBruijn initialBindingDepth sub) exp2)

  subst d sub (ELam ann arg expr) =
    -- Increase the depth as we move across a binding site
    ELam ann arg (subst (incrExprDepth d) (liftDeBruijn initialBindingDepth sub) expr)
