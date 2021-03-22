{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeFamilies       #-}

module Vehicle.Core.Check.Type where

import           Control.Exception (Exception)
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.State (MonadState(..))
import           Data.Functor.Foldable (fold)
import           Data.Text (Text)
import           Vehicle.Core.Check.Core
import           Vehicle.Core.Check.Builtin
import           Vehicle.Core.Type
import           Vehicle.Core.Abs (SortedName(..), Name(..))
import           Vehicle.Prelude

type KindEnv name builtin ann = [(Text, Kind name builtin ann)]
type TypeEnv name builtin ann = [(Text, Type name builtin ann)]

data DeBruijn (sort :: Sort) = DeBruijn
  { pos   :: Position
  , index :: Int
  }

data family Info (sort :: Sort)
data instance Info 'KIND = InfoKind
data instance Info 'TYPE = InfoType (Kind DeBruijn Builtin NoAnn)
data instance Info 'EXPR = InfoExpr (Type DeBruijn Builtin NoAnn)
data instance Info 'DECL = InfoDecl
data instance Info 'PROG = InfoProg
data instance Info 'TARG = InfoTArg (Kind DeBruijn Builtin NoAnn)
data instance Info 'EARG = InfoEArg (Type DeBruijn Builtin NoAnn)

check :: TCM m => SSort sort -> Info sort -> Tree sort SortedName Builtin NoAnn -> m (Tree sort DeBruijn Builtin Info)
check = undefined

infer :: TCM m => SSort sort -> Tree sort SortedName Builtin NoAnn -> m (Tree sort DeBruijn Builtin Info)
infer = undefined


{-
{-
class Check (tree :: (Sort -> *) -> (Sort -> *) -> (Sort -> *) -> *) where
  check :: TCM m => CheckResult -> tree SortedName Builtin NoAnn -> m (tree DeBruijn Builtin CheckResult)

class Infer (tree :: (Sort -> *) -> (Sort -> *) -> (Sort -> *) -> *) where
  infer :: TCM m => tree SortedName Builtin NoAnn -> m (tree DeBruijn Builtin CheckResult)
-}

-- -}
-- -}
-- -}
-- -}
-- -}
