import System.Environment (getArgs)
import Vehicle.Test.Golden.TestSpec.NewTestSpec (newTestSpec)

main :: IO ()
main = getArgs >>= newTestSpec
