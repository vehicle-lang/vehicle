{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Language.MetaSubstitution
  ( MetaSubstitution
  , singleton
  , lookup
  , map
  , insertWith
  ) where

import Prelude hiding (map, lookup)
import Data.Coerce (coerce)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print
import Vehicle.Language.Simplify

newtype MetaSubstitution = MetaSubstitution (IntMap CheckedExpr)
  deriving (Semigroup, Monoid)

instance Simplify MetaSubstitution where
  simplify (MetaSubstitution m) = MetaSubstitution <$> traverse simplify m

instance PrettyLang MetaSubstitution where
  prettyCore         (MetaSubstitution m) = pretty $ fmap prettyCore m
  prettyFrontend ctx (MetaSubstitution m) = pretty $ fmap (prettyFrontend ctx) m

singleton :: Meta -> CheckedExpr -> MetaSubstitution
singleton m e = coerce (IntMap.singleton (coerce m) e)

lookup :: Meta -> MetaSubstitution -> Maybe CheckedExpr
lookup m s = IntMap.lookup (coerce m) (coerce s)

map :: (CheckedExpr -> CheckedExpr) -> MetaSubstitution -> MetaSubstitution
map f = coerce. IntMap.map f . coerce

insertWith :: (CheckedExpr -> CheckedExpr -> CheckedExpr)
           -> Meta
           -> CheckedExpr
           -> MetaSubstitution
           -> MetaSubstitution
insertWith f m e s = coerce (IntMap.insertWith f (coerce m) e (coerce s))
