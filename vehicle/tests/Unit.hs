import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Encoding.UTF8 (utf8)
import Test.Tasty
  ( defaultMain,
    testGroup,
  )
import Vehicle.Test.Unit.Compile.CommandLine (commandLineParserTests)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "Tests"
      [ commandLineParserTests
      ]
