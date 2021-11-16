{-# LANGUAGE OverloadedStrings #-}

module Test.Generative where

import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import Data.Either (isRight)
import Debug.Trace (traceShow, trace, traceShowId)

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Type (typeCheck)

import Test.Tasty
import Test.Tasty.HUnit

import Test.Generative.Generator
import Test.Generative.ToVehicle
import Vehicle.Language.Print (prettyFriendly, prettyVerbose, prettyFriendlyDBClosed)

maxDepth :: Int
maxDepth = 15

annotate :: DeBruijnExpr () -> UncheckedExpr
annotate = fmap (const (mempty, TheMachine))

isWellTyped :: UncheckedExpr -> UncheckedExpr -> (Bool, [Message])
isWellTyped t e = (isRight result, logs)
  where
    prog           = Main [DefFun mempty (Identifier "_") t e]
    (result, logs) = runLogger (runExceptT (typeCheck prog))

testWellTyped :: Int -> Type Z -> Assertion
testWellTyped depth ty = do
  tms <- enumClosedTm depth ty
  forM_ tms $ \tm -> do
    let t = annotate $ convertType Star ty
    let e = annotate $ convertTerm ty tm
            -- (\x ->
            --   trace (layoutAsString (prettyVerbose x))
            --   trace (layoutAsString (prettyVerbose x))
            --   traceShow tm
            --   x)
    let (wellTyped, logs) = isWellTyped t e

    let errorMessage = (showMessages logs) <> "\n" <> (layoutAsString $
          "The expression" <+> squotes (prettyFriendly e) <+>
          "does not have type" <+> squotes (prettyFriendly t))

    assertBool errorMessage wellTyped

generativeTests :: TestTree
generativeTests = testGroup "Generative"
  [ testCase "typeChecking" (testWellTyped maxDepth (TyNat :-> TyNat))
  ]