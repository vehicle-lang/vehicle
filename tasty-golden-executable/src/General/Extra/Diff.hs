module General.Extra.Diff where

import Data.Algorithm.Diff (Diff, PolyDiff (..))

isBoth :: PolyDiff a b -> Bool
isBoth (Both {}) = True
isBoth _ = False

mapPolyDiff :: (a -> c) -> (b -> d) -> PolyDiff a b -> PolyDiff c d
mapPolyDiff f _ (First a) = First (f a)
mapPolyDiff _ g (Second b) = Second (g b)
mapPolyDiff f g (Both a b) = Both (f a) (g b)

mapDiff :: (a -> b) -> Diff a -> Diff b
mapDiff f = mapPolyDiff f f
