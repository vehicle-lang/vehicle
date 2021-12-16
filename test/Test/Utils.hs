

module Test.Utils
  ( textToCheckedExpr
  , retypeCheckExpr
  ) where

import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Data.Text
import Debug.Trace

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Compile (typeCheckExpr)
import Vehicle.Compile.Error.Meaningful
import Vehicle.Compile.Error
import Vehicle.Compile.Type (runTypeCheck)

-- If you want to see the logs for tests, change `discardLogger` to `traceLogger` here.
discardState :: ExceptT CompileError Logger a -> a
discardState e = case discardLogger $ runExceptT e of
  Left  x -> error (show (details x))
  Right y -> y

traceLogger :: Logger a -> a
traceLogger m =
  let (v, logs) = runLogger m in
  trace (showMessages logs) v

textToCheckedExpr :: Text -> CheckedExpr
textToCheckedExpr = discardState . typeCheckExpr

retypeCheckExpr :: CheckedExpr -> CheckedExpr
retypeCheckExpr = discardState . runTypeCheck