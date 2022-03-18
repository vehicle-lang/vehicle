module Test where

import Test.Tasty
import GHC.IO.Encoding

import Test.Compile.Golden as Compile (goldenTests)
import Test.Compile.Unit as Compile (unitTests)
import Test.Compile.Fail as Compile (failTests)
import Test.Check.Golden as Check (goldenTests)
-- import Test.Generative (generativeTests)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ compileTests
  --, checkTests
  ]

compileTests :: TestTree
compileTests = testGroup "Compile"
  [ Compile.goldenTests
  , Compile.unitTests
  , Compile.failTests
  ]

checkTests :: TestTree
checkTests = testGroup "Check"
  [ Check.goldenTests
  ]