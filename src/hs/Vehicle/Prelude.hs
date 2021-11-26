
module Vehicle.Prelude
  ( module X
  , VehicleLang(..)
  , ITP(..)
  , Verifier(..)
  , OutputTarget(..)
  , Negatable(..)
  , LogFilePath
  , (|->)
  , (!?)
  , (!!?)
  , rangeStart
  , repeatN
  , readRat
  , duplicate
  , oneHot
  , capitaliseFirstLetter
  , developerError
  ) where

import Data.Range
import Data.Text (Text, unpack)
import Data.Text qualified as Text
import Data.Bifunctor
import Numeric
import Control.Exception (Exception, throw)
import GHC.Stack (HasCallStack)

import Vehicle.Prelude.Token as X
import Vehicle.Prelude.Provenance as X
import Vehicle.Prelude.Prettyprinter as X
import Vehicle.Prelude.Logging as X
import Vehicle.Prelude.Supply as X

data VehicleLang = Frontend | Core
  deriving (Show)

data ITP
  = Agda
  deriving (Show, Read)

data Verifier
  = VNNLib
  | SMTLib
  deriving (Show, Read)

data OutputTarget
  = ITP ITP
  | Verifier Verifier

instance Show OutputTarget where
  show = \case
    ITP      arg -> show arg
    Verifier arg -> show arg

instance Read OutputTarget where
  readsPrec d x =
    case readsPrec d x of
      [] -> case readsPrec d x of
        []  -> []
        res -> fmap (first Verifier) res
      res -> fmap (first ITP) res

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

class Negatable a where
  neg :: a -> a

type LogFilePath = Maybe (Maybe FilePath)

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