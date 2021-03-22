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


check :: (IsToken tkName, TCM m) =>
         SSort sort ->
         Info sort ->
         Tree sort (K tkName) Builtin (K ()) ->
         m (Tree sort DeBruijn Builtin Info)
check = undefined


infer :: (IsToken tkName, TCM m) =>
         SSort sort ->
         Tree sort (K tkName) Builtin (K ()) ->
         m (Tree sort DeBruijn Builtin Info)
infer = undefined

-- | Essentially a no-op. Infers the empty 'KindInfo' for each constructor of
--   'Kind', and converts /no/ names to 'DeBruijn' indices.
--
--   NOTE: this function assumes kinds don't contain names and do not recurse
--   into any other sort of tree. If this assumption is violated, 'inferKind'
--   throws an 'UnexpectedSort' exception.
--
inferKind :: (IsToken tkName, TCM m) => Kind (K tkName) Builtin (K ()) -> m (Kind DeBruijn Builtin Info)
inferKind = mapTreeM unexpectedSort (const pure) fAnn SKIND
  where
    fAnn :: TCM m => forall sort. SSort sort -> (K ()) sort -> m (Info sort)
    fAnn SKIND = const (pure InfoKind)
    fAnn ssort = unexpectedSort ssort


inferType :: (IsToken tkName, TCM m) => Type (K tkName) Builtin (K ()) -> m (Type DeBruijn Builtin Info)
inferType = fold $ \case
  TForallF  _ n t   -> undefined
  TAppF     _ t1 t2 -> undefined
  TVarF     _ n     -> undefined
  TConF     _ op    -> undefined
  TLitDimF  _ d     -> undefined
  TLitListF _ ts    -> undefined
  TMetaF    _ i     -> undefined


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
