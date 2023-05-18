module Vehicle.Prelude.Misc where

-- NOTE: (Wen, 2022-11-01)
--       Moved these definitions here from Vehicle.Prelude,
--       so they can be used from Vehicle.Prelude.* modules.

import Control.Monad (when)
import Control.Monad.Identity (Identity (..))
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), NumberFormat (..))
import Data.Graph (Edge, Vertex, buildG, topSort)
import Data.IntMap (IntMap, updateLookupWithKey)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Vehicle.Prelude.Prettyprinter (Pretty (pretty))
import Vehicle.Syntax.AST

data VehicleLang = External | Internal
  deriving (Show)

-- | A textual representation of a Vehicle specification.
type SpecificationText = Text

-- | A set of properties in the specification.
type PropertyNames = [Name]

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

repeatN :: (a -> a) -> Int -> a -> a
repeatN _ 0 = id
repeatN f n = f . repeatN f (n - 1)

unzipWith :: (a -> (b, c)) -> [a] -> ([b], [c])
unzipWith f = unzip . map f

partitionMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m ([b], [a])
partitionMaybeM _ [] = return ([], [])
partitionMaybeM f (x : xs) = do
  res <- f x
  (as, bs) <- partitionMaybeM f xs
  return $ case res of
    Nothing -> (as, x : bs)
    Just y -> (y : as, bs)

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe f xs = runIdentity (partitionMaybeM (return . f) xs)

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = partitionMaybeM (\v -> do r <- f v; return $ if r then Just v else Nothing)

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

class Negatable a where
  neg :: a -> a

instance Negatable EqualityOp where
  neg Eq = Neq
  neg Neq = Eq

instance Negatable OrderOp where
  neg = \case
    Le -> Gt
    Lt -> Ge
    Ge -> Lt
    Gt -> Le

instance Negatable Quantifier where
  neg Forall = Exists
  neg Exists = Forall

-- | Used to distinguish between inputs and outputs of neural networks.
data InputOrOutput
  = Input
  | Output
  deriving (Show, Eq)

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

type TensorDimensions = [Int]

type TensorIndices = [Int]
