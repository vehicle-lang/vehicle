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
import Vehicle.Language.Print (prettyFriendly, prettyFriendlyDBClosed)

maxDepth :: Int
maxDepth = 15

annotate :: DeBruijnExpr () -> UncheckedExpr
annotate = fmap (const (mempty, TheMachine))

isWellTyped :: UncheckedExpr -> UncheckedExpr -> Bool
isWellTyped t e = isRight (discardLogger (runExceptT result))
  where
    prog   = Main [DefFun mempty (Identifier "_") t e]
    result = typeCheck prog

testWellTyped :: Int -> Type Z -> Assertion
testWellTyped depth ty = do
  tms <- enumClosedTm depth ty
  forM_ tms $ \tm -> do
    let t = annotate $ convertType Star ty
    let e = (\x ->
              trace (layoutAsString (prettyFriendlyDBClosed x))
              trace (layoutAsString (prettyFriendlyDBClosed x))
              traceShow tm
              x)
              (annotate $ convertTerm ty tm)
    let errorMessage = layoutAsString $
          "The expression" <+> prettyFriendlyDBClosed e <+>
          "does not have type" <+> prettyFriendlyDBClosed t

    assertBool errorMessage (isWellTyped t e)

generativeTests :: TestTree
generativeTests = testGroup "Generative"
  [ testCase "typeChecking" (testWellTyped maxDepth (TyNat :-> TyNat))
  ]