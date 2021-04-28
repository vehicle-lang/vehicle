{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
module Vehicle.Core.Normalise.Core
  ( NormError (..)
  , MonadNorm
  , NormExpr
  , NormType
  , NormDecl
  , NormProg
  , pattern EOp0
  , pattern EOp1
  , pattern EOp2
  , pattern EOp3
  , pattern ETrue
  , pattern EFalse
  , mkBool
  ) where

import Vehicle.Prelude (Position)
import Vehicle.Core.Type ( Sort(..), Tree (..), Expr, Type, Decl, Prog, K(..))
import Vehicle.Core.DeBruijn.Core (SortedDeBruijn(..))
import Control.Monad.Except (MonadError)
import Vehicle.Core.Abs (ExprName, Builtin(..))


-- |Errors thrown during normalisation
data NormError = MissingDefFunType ExprName
    | MalformedLambdaError
    | EmptyQuantifierDomain Position

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m = MonadError NormError m

-- |Some useful type synonyms
type NormExpr ann = Expr SortedDeBruijn (K Builtin) ann
type NormType ann = Type SortedDeBruijn (K Builtin) ann
type NormDecl ann = Decl SortedDeBruijn (K Builtin) ann
type NormProg ann = Prog SortedDeBruijn (K Builtin) ann

-- |Pattern synonyms to help matching during normalisation. Perhaps these are useful elsewhere and should be lifted?
pattern EOp0 op ann0 pos = ECon ann0 (K (Builtin (pos , op)))
pattern EOp1 op e1 ann0 ann1 pos = EApp ann1 (EOp0 op ann0 pos) e1
pattern EOp2 op e1 e2 ann0 ann1 ann2 pos = EApp ann2 (EOp1 op e1 ann0 ann1 pos) e2
pattern EOp3 op e1 e2 e3 ann0 ann1 ann2 ann3 pos  = EApp ann3 (EOp2 op e1 e2 ann0 ann1 ann2 pos) e3

pattern ETrue ann pos = EOp0 "true" ann pos
pattern EFalse ann pos = EOp0 "false" ann pos

mkBool :: Bool -> ann 'EXPR -> Position -> NormExpr ann
mkBool True ann pos = EOp0 "true" ann pos 
mkBool False ann pos = EOp0 "false" ann pos