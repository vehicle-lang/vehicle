module Vehicle.Backend.Marabou.Interact
  ( writeOutProperty
  ) where

import Control.Monad ( forM_ )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))

import Vehicle.Backend.Marabou.Core
import Vehicle.Backend.Prelude

writeOutProperty :: Maybe FilePath -> MarabouProperty -> IO ()
writeOutProperty filepath property = do
  -- Create a directory to store the queries
  forM_ filepath (createDirectoryIfMissing True)

  -- Write out the queries to the new directory
  let numberedQueries = zip [1..] (queries property)
  forM_ numberedQueries (writeQueryToFile filepath)

writeQueryToFile :: Maybe FilePath -> (Int, MarabouQuery) -> IO ()
writeQueryToFile filepath (queryID, query) = do
  let queryFilepath = fmap (</> "query" <> show queryID <.> "txt") filepath
  writeResultToFile MarabouBackend queryFilepath (doc query)