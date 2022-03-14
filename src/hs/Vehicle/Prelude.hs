
module Vehicle.Prelude
  ( module X
  , VehicleLang(..)
  , Negatable(..)
  , vehicleVersion
  , (|->)
  , (!?)
  , (!!?)
  , rangeStart
  , repeatN
  , readRat
  , duplicate
  , oneHot
  , partialSort
  , capitaliseFirstLetter
  , developerError
  , removeFileIfExists
  , fatalError
  , programOutput
  ) where

import Control.Monad.IO.Class
import Data.Range
import Data.Text (Text, unpack)
import Data.Text qualified as Text
import Data.Graph
import Data.Version (Version)
import Numeric
import Control.Exception (Exception, throw, catch, throwIO)
import GHC.Stack (HasCallStack)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)
import System.IO
import System.Exit

import Vehicle.Prelude.Token as X
import Vehicle.Prelude.Provenance as X
import Vehicle.Prelude.Prettyprinter as X
import Vehicle.Prelude.Logging as X
import Vehicle.Prelude.Supply as X

import Paths_vehicle qualified as Cabal (version)

vehicleVersion :: Version
vehicleVersion = Cabal.version

data VehicleLang = Frontend | Core
  deriving (Show)

infix 1 |->
-- | Useful for writing association lists.
(|->) :: a -> b -> (a, b)
(|->) = (,)

-- |Attempts to extract the first element from a bound
boundStart :: Bound a -> Maybe a
boundStart (Bound v Inclusive)= Just v
boundStart (Bound _v Exclusive)= Nothing

-- |Attempts to extract the first element in a range
rangeStart :: Range a -> Maybe a
rangeStart (SingletonRange a)          = Just a
rangeStart (SpanRange lb _ub)          = boundStart lb
rangeStart (LowerBoundRange b)         = boundStart b
rangeStart (UpperBoundRange _)         = Nothing
rangeStart InfiniteRange               = Nothing

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

readRat :: Text -> Rational
readRat str = case readFloat (Text.unpack str) of
  ((n, []) : _) -> n
  _             -> error "Invalid number"

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

--------------------------------------------------------------------------------
-- Developer errors

newtype DeveloperError = DeveloperError Text

instance Show DeveloperError where
  show (DeveloperError text) = unpack text

instance Exception DeveloperError

developerError :: HasCallStack => Doc a -> b
developerError message = throw $ DeveloperError $ layoutAsText $
  "Something went wrong internally. Please report the error" <+>
  "shown below to `https://github.com/wenkokke/vehicle/issues`." <> line <>
  "Error:" <+> message


--------------------------------------------------------------------------------
-- IO operations

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

fatalError :: MonadIO m => LoggingOptions -> Doc a -> m b
fatalError LoggingOptions{..} message = liftIO $ do
  hPrint errorHandle message
  exitFailure

programOutput :: MonadIO m => LoggingOptions -> Doc a -> m ()
programOutput LoggingOptions{..} message = liftIO $ hPrint outputHandle message