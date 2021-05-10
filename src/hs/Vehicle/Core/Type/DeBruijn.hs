{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vehicle.Core.Type.DeBruijn
  ( Idx
  , DeBruijn
  , idx
  , text
  ) where

import Data.Text (Text)
import Vehicle.Core.Type.Core (Sort(..), KnownSort(..), SSort(..))
import Vehicle.Prelude (In)

-- * Types

type Idx = Int

data DeBruijn (sort :: Sort) where
  TIdx :: Idx  -> DeBruijn 'TYPE
  EIdx :: Idx  -> DeBruijn 'EXPR
  TArg :: Text -> DeBruijn 'TARG
  EArg :: Text -> DeBruijn 'EARG

-- |Returns the index of a |DeBruijn|.
idx :: forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => DeBruijn sort -> Idx
idx = case sortSing :: SSort sort of
  STYPE -> \(TIdx i) -> i
  SEXPR -> \(EIdx i) -> i

-- |Returns the name of a |DeBruijn|.
text :: forall sort. (KnownSort sort, sort `In` ['TARG, 'EARG]) => DeBruijn sort -> Text
text = case sortSing :: SSort sort of
  STARG -> \(TArg n) -> n
  SEARG -> \(EArg n) -> n
