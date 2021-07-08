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

module Vehicle.Core.Compile.Subst
  ( subst
  ) where

import qualified Data.List.NonEmpty as NonEmpty (map)

import Vehicle.Core.AST

-- * DeBruijn substitution

class Subst (sort :: Sort) where
  substAcc :: BindingDepth    -- ^ current binding depth
    -> Tree DeBruijn ann sort -- ^ expression to substitute
    -> Tree DeBruijn ann sort -- ^ expression to substitute into
    -> Tree DeBruijn ann sort -- ^ the result of the substitution

  subst :: Tree DeBruijn ann sort -> Tree DeBruijn ann sort -> Tree DeBruijn ann sort
  subst = substAcc initialBindingDepth

instance Subst 'TYPE where
  substAcc _ _ expr@(TCon _ _)    = expr
  substAcc _ _ expr@(TLitDim _ _) = expr
  substAcc _ _ expr@(TMeta _ _)   = expr

  substAcc d sub (TApp ann fn arg)      = TApp ann (substAcc d sub fn) (substAcc d sub arg)
  substAcc d sub (TLitDimList ann typs) = TLitDimList ann (NonEmpty.map (substAcc d sub) typs)

  substAcc d sub (TForall ann optKind arg body) =
    -- Increase the depth as we move across a binding site
    TForall ann optKind arg (substAcc (incrTypeDepth d) (liftDeBruijn initialBindingDepth sub) body)

  substAcc d sub (TVar ann (TIndex i))  =
    case compare i (typeDepth d) of
      -- Index matches the expression we're substituting for
      EQ -> sub
      -- Index was bound in the original type
      LT -> TVar ann (TIndex i)
      -- Index was free in the original type, and we've removed a binder so decrease it by 1.
      GT -> TVar ann (TIndex (i - 1))

instance Subst 'EXPR where
  substAcc _ _ expr@(ELitInt _ _)  = expr
  substAcc _ _ expr@(ELitReal _ _) = expr
  substAcc _ _ expr@(ECon _ _)     = expr

  substAcc d sub (ELitSeq ann exprs)     = ELitSeq ann (NonEmpty.map (substAcc d sub) exprs)
  substAcc d sub (EAnn    ann expr typ)  = EAnn    ann (substAcc d sub expr) typ
  substAcc d sub (ETyLam  ann targ expr) = ETyLam  ann targ (substAcc d sub expr)
  substAcc d sub (ETyApp  ann expr typ)  = ETyApp  ann (substAcc d sub expr) typ
  substAcc d sub (EApp    ann exp1 exp2) = EApp    ann (substAcc d sub exp1) (substAcc d sub exp2)

  substAcc d sub (EVar ann (EIndex i)) =
    case compare i (exprDepth d) of
      -- Index matches the expression we're substituting for
      EQ -> sub
      -- Index was bound in the original expression
      LT -> EVar ann (EIndex i)
      -- Index was free in the original expression, and we've removed a binder so decrease it by 1.
      GT -> EVar ann (EIndex (i - 1))

  substAcc d sub (ELet ann arg exp1 exp2) =
    -- Increase the depth as we move across a binding site
    ELet ann arg (substAcc d sub exp1) (substAcc (incrExprDepth d) (liftDeBruijn initialBindingDepth sub) exp2)

  substAcc d sub (ELam ann arg expr) =
    -- Increase the depth as we move across a binding site
    ELam ann arg (substAcc (incrExprDepth d) (liftDeBruijn initialBindingDepth sub) expr)