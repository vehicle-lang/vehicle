import Vehicle.Test.Golden.TestSpec.New (newTestSpec)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= newTestSpec
