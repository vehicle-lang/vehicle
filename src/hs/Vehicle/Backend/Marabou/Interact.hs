module Vehicle.Backend.Marabou.Interact
  ( writeOutProperty
  ) where

import Control.Monad ( forM_ )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>), dropExtension)

import Vehicle.Backend.Marabou.Core
import Vehicle.Backend.Prelude

writeOutProperty :: Maybe FilePath -> MarabouProperty -> IO ()
writeOutProperty filepath property = do
  let directory = fmap dropExtension filepath

  -- Create a directory to store the queries
  forM_ directory (createDirectoryIfMissing True)

  -- Write out the queries to the new directory
  let numberedQueries = zip [1..] (queries property)
  forM_ numberedQueries (writeQueryToFile directory)

writeQueryToFile :: Maybe FilePath -> (Int, MarabouQuery) -> IO ()
writeQueryToFile directory (queryID, query) = do
  let queryFilepath = fmap (</> "query" <> show queryID <.> "txt") directory
  writeResultToFile MarabouBackend queryFilepath (doc query)