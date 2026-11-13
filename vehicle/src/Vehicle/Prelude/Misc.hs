{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Prelude.Misc where

#if MIN_VERSION_base(4,19,0)
import qualified Data.Functor as F
#endif

import Control.DeepSeq (NFData)
import Control.Monad (join, liftM2, when)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Class as State (MonadState (..), modify)
import Data.Aeson (FromJSON, Options (..), ToJSON (..), defaultOptions)
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), NumberFormat (..), encodePretty')
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Graph (Edge, Vertex, buildG, topSort)
import Data.Hashable (Hashable)
import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Serialize as Serialize (Get, Putter, Serialize (..), getListOf, putListOf)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.These (These (..))
import GHC.Generics (Generic)
import Numeric (readFloat, readSigned)
import System.Console.ANSI
import Text.EditDistance (defaultEditCosts, levenshteinDistance)
import Vehicle.Data.AST.Name (HasName, Name, nameOf)
import Vehicle.Prelude.Error (developerError)
import Vehicle.Prelude.Prettyprinter (Doc, Pretty (pretty))

data VehicleLang = External | Internal
  deriving (Show)

-- | A textual representation of a Vehicle module.
type ModuleText = Text

-- | A set of declarations in the specification.
type DeclarationNames = [Name]

(!?) :: (Eq a) => [(a, b)] -> a -> Maybe b
[] !? _ = Nothing
((k, v) : xs) !? k'
  | k == k' = Just v
  | otherwise = xs !? k'

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
(x : _) !!? 0 = Just x
(_ : xs) !!? i = xs !!? (i - 1)

count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where
    go n [] = n
    go n (x : xs)
      | p x = go (n + 1) xs
      | otherwise = go n xs

repeatN :: (a -> a) -> Int -> a -> a
repeatN _ 0 = id
repeatN f n = f . repeatN f (n - 1)

unzipWith :: (a -> (b, c)) -> [a] -> ([b], [c])
unzipWith f = unzipF . map f

traverseListLocal :: (MonadReader v m) => (a -> m (v -> v, b)) -> [a] -> m [b]
traverseListLocal f = \case
  [] -> return []
  x : xs -> do
    (update, y) <- f x
    ys <- local update (traverseListLocal f xs)
    return $ y : ys

partitionMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m ([b], [a])
partitionMaybeM _ [] = return ([], [])
partitionMaybeM f (x : xs) = do
  res <- f x
  (as, bs) <- partitionMaybeM f xs
  return $ case res of
    Nothing -> (as, x : bs)
    Just y -> (y : as, bs)

unionMaybeWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybeWith _ Nothing mb = mb
unionMaybeWith _ ma Nothing = ma
unionMaybeWith f (Just a) (Just b) = Just $ f a b

unionMaybeWithM :: (Monad m) => (a -> a -> m a) -> Maybe a -> Maybe a -> m (Maybe a)
unionMaybeWithM _ Nothing mb = return mb
unionMaybeWithM _ ma Nothing = return ma
unionMaybeWithM f (Just a) (Just b) = Just <$> f a b

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe f xs = runIdentity (partitionMaybeM (return . f) xs)

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = partitionMaybeM (\v -> do r <- f v; return $ if r then Just v else Nothing)

filterByIndex :: (Int -> Bool) -> [a] -> [a]
filterByIndex f xs = fmap snd $ filter (f . fst) $ zip [0 ..] xs

findFirstAndCycle :: forall a. (a -> Bool) -> [a] -> Maybe (a, [a])
findFirstAndCycle p xs = (\(v, as, bs) -> (v, bs <> as)) <$> go xs
  where
    go :: [a] -> Maybe (a, [a], [a])
    go = \case
      [] -> Nothing
      a : as
        | p a -> Just (a, [], as)
        | otherwise -> case go as of
            Nothing -> Nothing
            Just (v, bs, cs) -> Just (v, a : bs, cs)

countOccurrences :: (Ord a) => [a] -> Map a Int
countOccurrences = foldr (\v -> Map.insertWith (+) v 1) mempty

findDuplicates :: (Ord a) => [a] -> [(a, Int)]
findDuplicates xs = filter (\(_, n) -> n > 1) $ Map.toList $ countOccurrences xs

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

capitaliseFirstLetter :: Text -> Text
capitaliseFirstLetter name
  | Text.null name = name
  | otherwise =
      let (firstLetter, remainder) = Text.splitAt 1 name
       in Text.toUpper firstLetter <> remainder

iterateM :: (Monad m) => (a -> m a) -> a -> Int -> m a
iterateM _ e 0 = return e
iterateM f e n = f =<< iterateM f e (n - 1)

oneHot :: Int -> Int -> a -> [Maybe a]
oneHot i l x
  | i < 0 || l < i = error $ "Invalid arguments '" <> show i <> "' '" <> show l <> "'to `oneHot`"
  | i == 0 = Just x : replicate l Nothing
  | otherwise = Nothing : oneHot (i - 1) (l - 1) x

deleteAndGet :: (Ord a) => a -> Map a b -> (Maybe b, Map a b)
deleteAndGet = Map.updateLookupWithKey (\_ _ -> Nothing)

unionWithM :: (Monad m, Ord key) => (val -> val -> m val) -> Map key val -> Map key val -> m (Map key val)
unionWithM f m1 m2 = sequence $ Map.unionWith (\xm ym -> join $ liftM2 f xm ym) (Map.map return m1) (Map.map return m2)

mapKeysM :: (Monad m, Ord key) => (key -> m key) -> Map key val -> m (Map key val)
mapKeysM f xs = Map.fromList <$> traverse (bitraverse f pure) (Map.toList xs)

fromMappedKeyList :: (Ord key) => (key -> value) -> [key] -> Map key value
fromMappedKeyList f keys = Map.fromList $ fmap (\key -> (key, f key)) keys

fromMappedValueList :: (Ord key) => (value -> key) -> [value] -> Map key value
fromMappedValueList f values = Map.fromList $ fmap (\value -> (f value, value)) values

-- Base 4.16 once we upgrade
prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList ls ne = case ls of
  [] -> ne
  (x : xs) -> x :| xs <> NonEmpty.toList ne

alterKeys :: (Ord k) => Set k -> (a -> a) -> Map k a -> Map k a
alterKeys keys f xs = foldr (Map.alter (fmap f)) xs keys

partialSort :: forall a. (a -> a -> Maybe Ordering) -> [a] -> [a]
partialSort partialCompare xs = sortedNodes
  where
    edgesBetween :: (Vertex, a) -> (Vertex, a) -> [Edge]
    edgesBetween (k1, v1) (k2, v2) = case partialCompare v1 v2 of
      Nothing -> []
      Just LT -> [(k1, k2)]
      Just EQ -> [(k1, k2), (k2, k1)]
      Just GT -> [(k2, k1)]

    edgesFor :: [(Vertex, a)] -> [Edge]
    edgesFor [] = mempty
    edgesFor (v : vs) = concatMap (edgesBetween v) vs <> edgesFor vs

    graph = buildG (0, length xs - 1) (edgesFor (zip [0 ..] xs))
    sortedIndices = topSort graph
    sortedNodes = map (xs !!) sortedIndices

listOrd :: (a -> a -> Bool) -> [a] -> [a] -> Bool
listOrd _ [] [] = True
listOrd _ (_ : _) [] = False
listOrd _ [] (_ : _) = True
listOrd leq (x : xs) (y : ys) = le || (eq && listOrd leq xs ys)
  where
    le = leq x y && not (leq y x)
    eq = leq x y && leq y x

listIntersection :: (Ord a) => [a] -> [a] -> [a]
listIntersection xs ys = Set.toList $ Set.difference (Set.fromList xs) (Set.fromList ys)

findAndDeleteElem :: (a -> Bool) -> [a] -> Maybe (a, [a])
findAndDeleteElem p = go id
  where
    go _ [] = Nothing
    go prefix (x : xs)
      | p x = Just (x, prefix xs)
      | otherwise = go (prefix . (x :)) xs

traverseAndPair :: (Applicative m, Traversable f) => (a -> m b) -> f a -> m (f (a, b))
traverseAndPair f = traverse (\x -> (x,) <$> f x)

-- | Used to distinguish between inputs and outputs of neural networks.
data InputOrOutput
  = Input
  | Output
  deriving (Show, Eq, Ord, Generic)

instance NFData InputOrOutput

instance ToJSON InputOrOutput

instance FromJSON InputOrOutput

instance Hashable InputOrOutput

instance Pretty InputOrOutput where
  pretty = \case
    Input -> "input"
    Output -> "output"

xor :: Bool -> Bool -> Bool
xor p q = p /= q

dropIndices :: [Int] -> [a] -> [a]
dropIndices = go 0
  where
    go :: Int -> [Int] -> [a] -> [a]
    go _ [] xs = xs
    go _ _ [] = []
    go ci (i : is) (x : xs)
      | ci == i = go (ci + 1) is xs
      | otherwise = x : go (ci + 1) (i : is) xs

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

lookupEnumerable :: forall a b. (Bounded a, Enum a, Eq b) => (a -> b) -> b -> Maybe a
lookupEnumerable toKey target = do
  let values = enumerate @a
  find (\e -> toKey e == target) values

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond action = do
  c <- cond
  when c action

prettyJSONConfig :: Config
prettyJSONConfig =
  Config
    { confIndent = Spaces 2,
      confCompare = \t1 t2 -> compare t2 t1,
      confNumFormat = Generic,
      confTrailingNewline = False
    }

prettyAsJSON :: (ToJSON a) => a -> Doc b
prettyAsJSON x = pretty $ unpack $ encodePretty' prettyJSONConfig $ toJSON x

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { tagSingleConstructors = True
    }

readFloatAsRational :: Text -> Rational
readFloatAsRational str =
  case readSigned readFloat (Text.unpack str) of
    ((n, []) : _) -> n
    _ -> error "Invalid number"

setTextColour :: Color -> String -> String
setTextColour c s =
  join
    [setSGRCode [SetColor Foreground Vivid c], s, setSGRCode []]

cartesianProduct :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianProduct g xs ys = [g x y | x <- xs, y <- ys]

cartesianProductM :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
cartesianProductM g xs ys = sequence [g x y | x <- xs, y <- ys]

concatNonEmpty :: NonEmpty (NonEmpty a) -> NonEmpty a
concatNonEmpty ((x :| xs) :| xss) = x :| (xs <> concatMap NonEmpty.toList xss)

nonEmptyCartesianProductM :: (Monad m) => (a -> b -> m c) -> NonEmpty a -> NonEmpty b -> m (NonEmpty c)
nonEmptyCartesianProductM f (x :| xs) (y :| ys) = do
  z <- f x y
  zs <- traverse (f x) ys
  zss <- cartesianProductM f xs (y : ys)
  return $ z :| (zs <> zss)

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o2 = o2
thenCmp o1 _ = o1

mergeNonEmptyKeyValues :: (Ord a) => (NonEmpty b -> b) -> NonEmpty (a, b) -> NonEmpty (a, b)
mergeNonEmptyKeyValues f xs = do
  let results = Map.toList $ Map.fromListWith (<>) $ NonEmpty.toList $ fmap (second (:| [])) xs
  case results of
    [] -> developerError "impossible"
    u : us -> fmap (second f) (u :| us)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

eitherM :: (a -> m c) -> (b -> m c) -> Either a b -> m c
eitherM f g = \case
  Left x -> f x
  Right y -> g y

theseErrors :: (a -> b -> c) -> Either e1 a -> Either e2 b -> Either (These e1 e2) c
theseErrors f v1 v2 = case (v1, v2) of
  (Left e1, Left e2) -> Left $ These e1 e2
  (Left e1, Right {}) -> Left $ This e1
  (Right {}, Left e2) -> Left $ That e2
  (Right r1, Right r2) -> Right $ f r1 r2

localState :: (MonadState s m) => (s -> s) -> m a -> m a
localState f action = do
  originalState <- State.get
  modify f
  result <- action
  State.put originalState
  return result

unzipF :: (Functor f) => f (a, b) -> (f a, f b)
#if MIN_VERSION_base(4,19,0)
unzipF = F.unzip
#else
unzipF = NonEmpty.unzip
#endif

foldrM1 :: (Monad m) => (t -> t -> m t) -> NonEmpty t -> m t
foldrM1 _ (x :| []) = pure x
foldrM1 f (x :| z : xs) = do
  y <- foldrM1 f (z :| xs)
  f x y

--------------------------------------------------------------------------------
-- Constants

readNat :: Text -> Int
readNat = read . Text.unpack

readRat :: Text -> Prelude.Rational
readRat str = case readFloat (Text.unpack str) of
  ((n, []) : _) -> n
  _ -> developerError "Invalid number"

--------------------------------------------------------------------------------
-- Spelling

mispellingsSortedByLikelihood :: (HasName object Name) => object -> [object] -> [object]
mispellingsSortedByLikelihood symbol possibilities = do
  let scoredPossibilities = mapMaybe (symbol `isMispellingOf`) possibilities
  let finalPossibilities = sortOn snd scoredPossibilities
  fmap fst finalPossibilities

isMispellingOf :: (HasName object Name) => object -> object -> Maybe (object, Int)
isMispellingOf symbol possibility = do
  let fieldName = Text.unpack $ nameOf symbol
  let distance = levenshteinDistance defaultEditCosts fieldName (Text.unpack $ nameOf possibility)
  if distance <= length fieldName `div` 2
    then Just (possibility, distance)
    else Nothing

--------------------------------------------------------------------------------
-- Serialization instances missing from Cereal

instance (Serialize a) => Serialize (NonEmpty a) where
  put = putNonEmptyListOf Serialize.put
  get = getNonEmptyListOf Serialize.get

getNonEmptyListOf :: Get a -> Get (NonEmpty a)
getNonEmptyListOf m = do
  xs <- Serialize.getListOf m
  case NonEmpty.nonEmpty xs of
    Nothing -> fail "getNonEmptyListOf: empty list"
    Just neList -> pure neList

putNonEmptyListOf :: Putter a -> Putter (NonEmpty a)
putNonEmptyListOf pa = Serialize.putListOf pa . NonEmpty.toList
