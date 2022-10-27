import System.Process (callCommand)

main :: IO ()
main = do
  callCommand "vehicle --version"