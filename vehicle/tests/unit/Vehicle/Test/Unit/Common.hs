{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Test.Unit.Common where

import Control.Monad.Except (ExceptT)
import Data.Data (Proxy (..))
import Data.Functor.Foldable (Recursive (cata))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Tagged (Tagged (Tagged))
import Debug.Trace (trace)
import Test.Tasty (TestTree, askOption, includingOptions)
import Test.Tasty.HUnit (Assertion, testCase)
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Options (IsOption (..), OptionDescription (Option))
import Text.Read (readMaybe)
import Vehicle.Compile.Error (CompileError, MonadCompile)
import Vehicle.Compile.Error.Message
  ( MeaningfulError (details),
    logCompileError,
  )
import Vehicle.Compile.Normalise (nfTypeClassOp)
import Vehicle.Compile.Prelude
  ( Builtin (TypeClassOp),
    Expr (..),
    ExprF (..),
    LoggingLevel,
    TypeCheckedExpr,
    normApp,
  )
import Vehicle.Prelude
  ( DelayedLoggerT,
    Pretty (pretty),
    defaultLoggingLevel,
    developerError,
    loggingLevelHelp,
    runDelayedLoggerT,
    showMessages,
  )

vehicleLoggingIngredient :: Ingredient
vehicleLoggingIngredient =
  includingOptions [Option (Proxy :: Proxy LoggingLevel)]

instance IsOption LoggingLevel where
  defaultValue :: LoggingLevel
  defaultValue = defaultLoggingLevel

  parseValue :: String -> Maybe LoggingLevel
  parseValue = readMaybe

  optionName :: Tagged LoggingLevel String
  optionName = Tagged "vehicle-logging"

  optionHelp :: Tagged LoggingLevel String
  optionHelp = Tagged loggingLevelHelp

--------------------------------------------------------------------------------
-- Test settings monad

unitTestCase :: String -> ExceptT CompileError (DelayedLoggerT IO) Assertion -> TestTree
unitTestCase testName errorOrAssertionWithLogs =
  askOption $ \logLevel -> testCase testName (traceLogs logLevel errorOrAssertionWithLogs)
  where
    traceLogs :: LoggingLevel -> ExceptT CompileError (DelayedLoggerT IO) Assertion -> Assertion
    traceLogs logLevel e = do
      let e' = logCompileError e
      (v, logs) <- runDelayedLoggerT logLevel e'
      let result = if null logs then v else trace (showMessages logs) v
      case result of
        Left x -> developerError $ pretty $ details x
        Right y -> y

normTypeClasses :: MonadCompile m => TypeCheckedExpr -> m TypeCheckedExpr
normTypeClasses = cata $ \case
  AppF p fun args -> do
    fun' <- fun
    args' <- traverse sequenceA args
    case fun' of
      Builtin p' (TypeClassOp op) -> case nfTypeClassOp p' op (NonEmpty.toList args') of
        Nothing -> error "No metas should be present"
        Just res -> do
          (fn, newArgs) <- res
          return $ normApp p fn newArgs
      _ -> return $ App p fun' args'
  UniverseF p l -> return $ Universe p l
  HoleF p n -> return $ Hole p n
  MetaF p m -> return $ Meta p m
  LiteralF p l -> return $ Literal p l
  BuiltinF p op -> return $ Builtin p op
  AnnF p e t -> Ann p <$> e <*> t
  PiF p binder result -> Pi p <$> sequenceA binder <*> result
  LetF p bound binder body -> Let p <$> bound <*> sequenceA binder <*> body
  LamF p binder body -> Lam p <$> sequenceA binder <*> body
  LVecF p xs -> LVec p <$> sequence xs
  VarF p v -> return $ Var p v
