{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Vehicle.Core.AST.DeBruijn
  ( Index
  , Name(..)
  , DeBruijn(..)
  , fromIndex
  , toIndex
  , fromSymbol
  , toSymbol
  , toName
  , BindingDepth(..)
  , incrTypeDepth
  , incrExprDepth
  , initialBindingDepth
  , DeBruijnLifting(..)
  ) where

import qualified Data.List.NonEmpty as NonEmpty (map)

import Vehicle.Prelude
import Vehicle.Core.AST.Core

-- * Types

data Name
  = User Symbol
  | Machine

deriving instance Eq Name

type Index = Int

data DeBruijn (sort :: Sort) where
  TIndex  :: Index -> DeBruijn 'TYPE
  EIndex  :: Index -> DeBruijn 'EXPR
  TSymbol :: Name  -> DeBruijn 'TARG
  ESymbol :: Name  -> DeBruijn 'EARG


-- |Wraps an |Index| as a |DeBruijn|.
fromIndex :: forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => Index -> DeBruijn sort
fromIndex = case sortSing :: SSort sort of
  STYPE -> TIndex
  SEXPR -> EIndex

-- |Returns the |Index| of a |DeBruijn|.
toIndex :: forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => DeBruijn sort -> Index
toIndex (TIndex index) = index
toIndex (EIndex index) = index

-- |Wraps a |Symbol| as a |DeBruijn|.
fromSymbol :: forall sort. (KnownSort sort, sort `In` ['TARG, 'EARG]) => Symbol -> DeBruijn sort
fromSymbol = case sortSing :: SSort sort of
  STARG -> TSymbol . User
  SEARG -> ESymbol . User

-- |Returns the |Symbol| of a |DeBruijn|.
toSymbol :: forall sort. (KnownSort sort, sort `In` ['TARG, 'EARG]) => DeBruijn sort -> Maybe Symbol
toSymbol (TSymbol (User symbol)) = Just symbol
toSymbol (ESymbol (User symbol)) = Just symbol
toSymbol _                       = Nothing

toName :: forall sort. (KnownSort sort, sort `In` ['TARG, 'EARG]) => DeBruijn sort -> Name
toName (TSymbol name) = name
toName (ESymbol name) = name

deriving instance Eq (DeBruijn sort)


data BindingDepth = BindingDepth { typeDepth :: Int , exprDepth :: Int}

incrTypeDepth :: BindingDepth -> BindingDepth
incrTypeDepth (BindingDepth t e) = BindingDepth (t + 1) e

incrExprDepth :: BindingDepth -> BindingDepth
incrExprDepth (BindingDepth t e) = BindingDepth t (e + 1)

initialBindingDepth :: BindingDepth
initialBindingDepth = BindingDepth 0 0


-- Implementation of substitution and lifting for De Bruijn indexed terms.
-- Code loosely based off of:
-- http://blog.discus-lang.org/2011/08/how-i-learned-to-stop-worrying-and-love.html

-- TODO condense down to a single case per SORT in the same pattern as the scope checker?
-- (using Dataflow monad?)

-- * DeBruijn lifting

class DeBruijnLifting (sort :: Sort) where
  -- | liftDeBruijn all deBruin indices that refer to environment variables by 1.
  liftDeBruijn :: BindingDepth           -- ^ current binding depth
               -> Tree DeBruijn ann sort -- ^ expression containing the variable references to lift
               -> Tree DeBruijn ann sort -- ^ the result of the lifting

instance DeBruijnLifting 'TYPE where
  liftDeBruijn _ expr@(TCon _ _) = expr
  liftDeBruijn _ expr@(TLitDim _ _) = expr
  liftDeBruijn _ expr@(TMeta _ _) = expr

  liftDeBruijn d (TApp ann fn arg) = TApp ann (liftDeBruijn d fn) (liftDeBruijn d arg)
  liftDeBruijn d (TLitDimList ann typs) = TLitDimList ann (NonEmpty.map (liftDeBruijn d) typs)

  liftDeBruijn d (TForall ann arg body) = TForall ann arg
    -- Increase the depth as we move across a binding site
    (liftDeBruijn (incrTypeDepth d) body)

  liftDeBruijn d (TVar ann (TIndex i)) = TVar ann (TIndex i')
    where
      i' | typeDepth d <= i = i + 1 -- Index is referencing the environment so increment it
         | otherwise        = i     -- Index is locally bound so no need to increment it

instance DeBruijnLifting 'EXPR where
  liftDeBruijn _ expr@(ELitInt _ _)  = expr
  liftDeBruijn _ expr@(ELitReal _ _) = expr
  liftDeBruijn _ expr@(ECon _ _)     = expr

  liftDeBruijn d (EApp    ann exp1 exp2) = EApp ann (liftDeBruijn d exp1) (liftDeBruijn d exp2)
  liftDeBruijn d (ELitSeq ann exprs)     = ELitSeq ann (NonEmpty.map (liftDeBruijn d) exprs)
  liftDeBruijn d (EAnn    ann expr typ)  = EAnn ann (liftDeBruijn d expr) typ
  liftDeBruijn d (ETyLam  ann targ expr) = ETyLam ann targ (liftDeBruijn d expr)
  liftDeBruijn d (ETyApp  ann expr typ)  = ETyApp ann (liftDeBruijn d expr) typ

  liftDeBruijn d (EVar ann (EIndex i)) = EVar ann (EIndex i')
    where
      i' | exprDepth d <= i = i + 1 -- Index is referencing the environment so increment it
         | otherwise        = i     -- Index is locally bound so no need to increment it

  liftDeBruijn d (ELet ann arg exp1 exp2) = ELet ann arg
    -- Maintain the current depth as move across the let bound expression
    (liftDeBruijn d exp1)
    -- Increase the current depth as we move across the variable
    (liftDeBruijn (incrExprDepth d) exp2)

  liftDeBruijn d (ELam ann arg expr) = ELam ann arg
    -- Increase the current depth as we move across a lambda.
    (liftDeBruijn (incrExprDepth d) expr)
