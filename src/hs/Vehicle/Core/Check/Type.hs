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

-- | Essentially a no-op. Infers the empty 'KindInfo' for each constructor of
--   'Kind', and converts /no/ names to 'DeBruijn' indices.
--
--   NOTE: this function assumes kinds don't contain names and do not recurse
--   into any other sort of tree. If this assumption is violated, 'inferKind'
--   throws an 'UnexpectedSort' exception.
--
inferKind :: TCM m => Kind SortedName Builtin NoAnn -> m (Kind DeBruijn Builtin Info)
inferKind = mapTreeM unexpectedSort (const pure) fAnn SKIND
  where
    fAnn :: TCM m => forall sort. SSort sort -> NoAnn sort -> m (Info sort)
    fAnn SKIND = const (pure InfoKind)
    fAnn ssort = unexpectedSort ssort

inferType :: TCM m => Type SortedName Builtin NoAnn -> m (Type DeBruijn Builtin Info)
inferType = fold $ \case
  TForallF  _NA n t   -> undefined
  TAppF     _NA t1 t2 -> undefined
  TVarF     _NA n     -> undefined
  TConF     _NA op    -> undefined
  TLitDimF  _NA d     -> undefined
  TLitListF _NA ts    -> undefined
  TMetaF    _NA i     -> undefined

-- | Throws an 'UnexpectedSort' exception for any sorted type.
--
--   NOTE: this function is useful for handling the cases which we /know/ do not
--   occur when using 'mapTree' or 'mapTreeM', e.g., a type recursing back into
--   a declaration.
--
unexpectedSort :: TCM m => SSort sort -> sorted sort -> m (sorted' sort)
unexpectedSort ssort _ = throwError (UnexpectedSort (toSort ssort))

-- -}
-- -}
-- -}
-- -}
-- -}
