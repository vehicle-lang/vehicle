
module Vehicle.Prelude
  ( module X
  , VehicleLang(..)
  , SpecificationText
  , PropertyNames
  , DeclarationNames
  , Negatable(..)
  , InputOrOutput(..)
  , vehicleVersion
  , (|->)
  , (!?)
  , (!!?)
  , rangeStart
  , repeatN
  , readNat
  , readRat
  , deleteAndGet
  , duplicate
  , oneHot
  , partialSort
  , capitaliseFirstLetter
  , removeFileIfExists
  , fatalError
  , programOutput
  , partitionM
  , prependList
  , xor
  , enumerate
  ) where

import Control.Exception (catch, throwIO)
import Control.Monad.IO.Class
import Data.Graph
import Data.IntMap (IntMap, updateLookupWithKey)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Range
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version (Version)
import Numeric
import System.Directory (removeFile)
import System.Exit
import System.IO
import System.IO.Error (isDoesNotExistError)

import Vehicle.Prelude.DeveloperError as X
import Vehicle.Prelude.IO as X
import Vehicle.Prelude.Logging as X
import Vehicle.Prelude.Prettyprinter as X
import Vehicle.Prelude.Supply as X
import Vehicle.Prelude.Token as X

import Paths_vehicle qualified as Cabal (version)

vehicleVersion :: Version
vehicleVersion = Cabal.version

data VehicleLang = External | Internal
  deriving (Show)

-- | A textual representation of a Vehicle specification.
type SpecificationText = Text

-- | A set of properties in the specification.
type PropertyNames = Set Name

-- | A set of declarations in the specification.
type DeclarationNames = Set Name

infix 1 |->
-- | Useful for writing association lists.
(|->) :: a -> b -> (a, b)
(|->) = (,)

-- |Attempts to extract the first element from a bound
boundStart :: Bound a -> Maybe a
boundStart (Bound v Inclusive)= Just v
boundStart (Bound _v Exclusive)=Nothing

-- |Attempts to extract the first element in a range
rangeStart :: Range a -> Maybe a
rangeStart (SingletonRange a)  = Just a
rangeStart (SpanRange lb _ub)  = boundStart lb
rangeStart (LowerBoundRange b) = boundStart b
rangeStart (UpperBoundRange _) = Nothing
rangeStart InfiniteRange       = Nothing

(!?) :: Eq a => [(a,b)] -> a -> Maybe b
[] !? _ = Nothing
((k , v) : xs) !? k'
  | k == k'   = Just v
  | otherwise = xs !? k'

(!!?) :: [a] -> Int -> Maybe a
[] !!? _       = Nothing
(x : _)  !!? 0 = Just x
(_ : xs) !!? i = xs !!? (i - 1)

repeatN :: (a -> a) -> Int -> a -> a
repeatN _ 0 = id
repeatN f n = f . repeatN f (n-1)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    pure ([x | res]++as, [x | not res]++bs)

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

capitaliseFirstLetter :: Text -> Text
capitaliseFirstLetter name
  | Text.null name = name
  | otherwise =
    let (firstLetter, remainder) = Text.splitAt 1 name in
      Text.toUpper firstLetter <> remainder

oneHot :: Int -> Int -> a -> [Maybe a]
oneHot i l x
  | i < 0 || l < i = error $ "Invalid arguments '" <> show i <> "' '" <> show l <> "'to `oneHot`"
  | i == 0         = Just x  : replicate l Nothing
  | otherwise      = Nothing : oneHot (i-1) (l-1) x

readNat :: Text -> Int
readNat = read . Text.unpack

readRat :: Text -> Rational
readRat str = case readFloat (Text.unpack str) of
  ((n, []) : _) -> n
  _             -> error "Invalid number"

deleteAndGet :: Int -> IntMap a -> (Maybe a, IntMap a)
deleteAndGet = updateLookupWithKey (\_ _ -> Nothing)

-- Base 4.16 once we upgrade
prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList ls ne = case ls of
  []       -> ne
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
    edgesFor []       = mempty
    edgesFor (v : vs) = concatMap (edgesBetween v) vs <> edgesFor vs

    graph = buildG (0, length xs - 1) (edgesFor (zip [0..] xs))
    sortedIndices = topSort graph
    sortedNodes   = map (xs !!) sortedIndices

class Negatable a where
  neg :: a -> a

-- | Used to distinguish between inputs and outputs of neural networks.
data InputOrOutput
  = Input
  | Output
  deriving (Show, Eq)

instance Pretty InputOrOutput where
  pretty = \case
    Input  -> "input"
    Output -> "output"

xor :: Bool -> Bool -> Bool
xor p q = p /= q

--------------------------------------------------------------------------------
-- IO operations

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

fatalError :: MonadIO m => VehicleIOSettings -> Doc a -> m b
fatalError VehicleIOSettings{..} message = liftIO $ do
  hPrint errorHandle message
  exitFailure

programOutput :: MonadIO m => VehicleIOSettings -> Doc a -> m ()
programOutput VehicleIOSettings{..} message = liftIO $ hPrint outputHandle message

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound..maxBound]
