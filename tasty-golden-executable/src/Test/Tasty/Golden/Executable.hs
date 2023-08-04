{-# LANGUAGE PatternSynonyms #-}

module Test.Tasty.Golden.Executable
  ( SomeOption (..),
    External (..),
    Ignore (..),
    IgnoreFiles (..),
    makeTestTreeFromFile,
    makeTestTreeFromDirectoryRecursive,
  )
where

import General.Extra.Option (SomeOption (..))
import Test.Tasty.Golden.Executable.TestSpec.External (External (..))
import Test.Tasty.Golden.Executable.TestSpec.Ignore (Ignore (..), IgnoreFiles (..))
import Test.Tasty.Golden.Executable.TestSpecs (makeTestTreeFromDirectoryRecursive, makeTestTreeFromFile)
