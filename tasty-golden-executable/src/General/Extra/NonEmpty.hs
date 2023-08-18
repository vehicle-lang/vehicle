{-# LANGUAGE CPP #-}

module General.Extra.NonEmpty
  ( appendList,
    prependList,
    singleton,
  )
where

#if MIN_VERSION_base(4,16,0)
import Data.List.NonEmpty (appendList, prependList, singleton)
#else
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty (nonEmpty)

singleton :: a -> NonEmpty a
singleton x = x :| []

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList xs ys = maybe ys (<> ys) (NonEmpty.nonEmpty xs)

appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList xs ys = maybe xs (xs <>) (NonEmpty.nonEmpty ys)
#endif
