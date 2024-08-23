{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid NonEmpty.unzip" #-}
module Vehicle.Prelude.Misc where

import Control.DeepSeq (NFData)
import Control.Monad (join, when)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), modify)
import Data.Aeson (FromJSON, Options (..), ToJSON, defaultOptions)
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), NumberFormat (..))
import Data.Graph (Edge, Vertex, buildG, topSort)
import Data.Hashable (Hashable)
import Data.IntMap (IntMap, updateLookupWithKey)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Numeric (readFloat, readSigned)
import System.Console.ANSI
import Vehicle.Prelude.Prettyprinter (Pretty (pretty))
import Vehicle.Syntax.AST.Name (Name)

data VehicleLang = External | Internal
  deriving (Show)

-- | A textual representation of a Vehicle specification.
type SpecificationText = Text

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
unzipWith f = unzip . map f

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

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe f xs = runIdentity (partitionMaybeM (return . f) xs)

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = partitionMaybeM (\v -> do r <- f v; return $ if r then Just v else Nothing)

filterByIndex :: (Int -> Bool) -> [a] -> [a]
filterByIndex f xs = fmap snd $ filter (f . fst) $ zip [0 ..] xs

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

oneHot :: Int -> Int -> a -> [Maybe a]
oneHot i l x
  | i < 0 || l < i = error $ "Invalid arguments '" <> show i <> "' '" <> show l <> "'to `oneHot`"
  | i == 0 = Just x : replicate l Nothing
  | otherwise = Nothing : oneHot (i - 1) (l - 1) x

deleteAndGet :: Int -> IntMap a -> (Maybe a, IntMap a)
deleteAndGet = updateLookupWithKey (\_ _ -> Nothing)

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

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

supportedOptions :: [String] -> String
supportedOptions opts = "Supported options: " <> List.intercalate ", " opts

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
cartesianProduct f xs ys = [f x y | x <- xs, y <- ys]

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o2 = o2
thenCmp o1 _ = o1

getModify :: (MonadState s m) => (s -> s) -> m s
getModify f = do
  x <- get
  modify f
  return x

--------------------------------------------------------------------------------
-- Constants

-- At the moment we only support rational coefficients.
type Coefficient = Rational

-- If we're ever to support matrix multiplication we'll need to make this a full
-- field structure.
--
-- However, we run into issues that we can't define a `zero` element as we don't
-- don't have access to the dimensions of tensors at the type level due to the
-- lack of dependent types.
class IsConstant constant where
  isZero :: constant -> Bool
  scaleConstant :: Coefficient -> constant -> constant
  addConstants :: Coefficient -> Coefficient -> constant -> constant -> constant

instance IsConstant Rational where
  isZero = (== 0.0)
  scaleConstant = (*)
  addConstants a b x y = a * x + b * y
