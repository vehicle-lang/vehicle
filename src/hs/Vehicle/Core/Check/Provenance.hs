{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Vehicle.Core.Check.Provenance where

import Data.Functor.Foldable (fold)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Vehicle.Core.Check.Core
import Vehicle.Core.Type
import Vehicle.Prelude

data Provenance
  = Range Position Position
  | Implicit
  deriving (Eq, Ord, Show)

instance Semigroup Provenance where
  (<>) = undefined

toProvenance :: IsToken a => a -> Provenance
toProvenance tk = Range p p where p = tkPos tk

moveProvenance :: (IsToken tkName, IsToken tkBuiltin, TCM m) =>
                  SSort sort ->
                  Tree sort (K tkName) (K tkBuiltin) ann ->
                  m (Tree sort (K tkName) (K tkBuiltin) (K Provenance :*: ann))
moveProvenance SKIND = fold $ \case
  KAppF  ann k1 k2 -> undefined
  KConF  ann tk    -> pure $ KCon (K (toProvenance tk) :*: ann) tk
  KMetaF ann i     -> pure $ KMeta (K Implicit :*: ann) i
moveProvenance STYPE = undefined
moveProvenance STARG = undefined
moveProvenance SEXPR = undefined
moveProvenance SEARG = undefined
moveProvenance SDECL = undefined
moveProvenance SPROG = undefined
