{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Vehicle.Core.Normalise.Core
  ( NormError (..)
  , MonadNorm
  , pattern EOp0
  , pattern EOp1
  , pattern EOp2
  , pattern EOp3
  , mkBool
  ) where

import Control.Monad.Except (MonadError)
import Vehicle.Core.AST
import Vehicle.Prelude ( Provenance )

-- |Errors thrown during normalisation
data NormError
  = MissingDefFunType _        -- TODO: should be a type error?
  | MalformedLambdaError       -- TODO: should be a type error?
  | EmptyQuantifierDomain Provenance

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m = MonadError NormError m

-- TODO: migrate to module with builtins

-- |Pattern synonyms to help matching during normalisation. Perhaps these are useful elsewhere and should be lifted?
pattern EOp0 op ann0 = Builtin ann0 op
pattern EOp1 op e1 ann0 ann1 = App ann1 (EOp0 op ann0) e1
pattern EOp2 op e1 e2 ann0 ann1 ann2 = App ann2 (EOp1 op e1 ann0 ann1) e2
pattern EOp3 op e1 e2 e3 ann0 ann1 ann2 ann3 = App ann3 (EOp2 op e1 e2 ann0 ann1 ann2) e3

-- TODO: migrate to different module?

mkBool :: Bool -> ann -> DeBruijnExpr ann
mkBool b ann = Literal ann (LitBool b)
