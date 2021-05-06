{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}

module Vehicle.Core.Check.Type where

import           Control.Exception (Exception)
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.State (MonadState(..))
import           Data.Functor.Foldable (fold)
import           Vehicle.Core.Check.Core
import           Vehicle.Core.Check.Builtin
import           Vehicle.Core.Type
import           Vehicle.Prelude

data DeBruijn (sort :: Sort) = DeBruijn
  { pos   :: Position
  , index :: Int
  }

data family Info (sort :: Sort)
data instance Info 'KIND = InfoKind
data instance Info 'TYPE = InfoType (Kind DeBruijn Builtin (K ()))
data instance Info 'TARG = InfoTArg (Kind DeBruijn Builtin (K ()))
data instance Info 'EXPR = InfoExpr (Type DeBruijn Builtin (K ()))
data instance Info 'EARG = InfoEArg (Type DeBruijn Builtin (K ()))
data instance Info 'DECL = InfoDecl
data instance Info 'PROG = InfoProg


check :: (IsToken tkName, KnownSort sort, TCM m) =>
         Info sort ->
         Tree (K tkName) Builtin (K ()) sort ->
         m (Tree DeBruijn Builtin Info sort)
check = undefined


infer :: (IsToken tkName, KnownSort sort, TCM m) =>
         Tree (K tkName) Builtin (K ()) sort ->
         m (Tree DeBruijn Builtin Info sort)
infer = undefined

-- -}
-- -}
-- -}
-- -}
-- -}
