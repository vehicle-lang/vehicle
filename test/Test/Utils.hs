module Test.Utils
  ( SpecLocation(..)
  , locationDir
  , textToCheckedExpr
  , retypeCheckExpr
  ) where

import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Data.Text
import Debug.Trace
import System.FilePath ((</>))

import Test.Tasty ( TestName, testGroup, after, DependencyType )
import Test.Tasty.Runners (TestTree(..))

import Vehicle.Compile (typeCheckExpr)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.Type (runTypeCheck)


data SpecLocation
  = Tests
  | Examples

locationDir :: SpecLocation -> FilePath
locationDir Tests    = "test" </> "specs"
locationDir Examples = "examples"


-- If you want to see the logs for tests, change `discardWarningsAndLogs` to
-- `traceLogger` here.
discardState :: ExceptT CompileError Logger a -> a
discardState e = case discardWarningsAndLogs $ logCompileError e of
  Left  x -> developerError $ pretty $ details x
  Right y -> y

traceLogger :: Logger a -> a
traceLogger m =
  let (v, logs) = runLogger m in
  trace (showMessages logs) v

textToCheckedExpr :: Text -> CheckedExpr
textToCheckedExpr = discardState . typeCheckExpr

retypeCheckExpr :: CheckedExpr -> CheckedExpr
retypeCheckExpr = discardState . runTypeCheck