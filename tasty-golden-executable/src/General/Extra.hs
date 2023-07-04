module General.Extra where

import Data.Char (isSpace)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)

-- | Find the duplicate elements in a list:
duplicates :: (Eq a, Hashable a) => [a] -> [a]
duplicates = duplicatesAcc HashSet.empty
  where
    duplicatesAcc _seen [] = []
    duplicatesAcc seen (x : xs)
      | x `HashSet.member` seen = x : duplicatesAcc seen xs
      | otherwise = duplicatesAcc (HashSet.insert x seen) xs

-- | If the boolean condition holds, return `Just a`.
--   Otherwise, return `Nothing`.
boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe just
  | just = Just
  | otherwise = const Nothing

-- | Split a String based on a predicate.
splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile isSpace s of
  "" -> []
  s' -> w : splitOn p s''
    where
      (w, s'') = break p s'
