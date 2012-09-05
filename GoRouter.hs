import System.Environment
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad

type LookupFn = [String] -> Maybe String

httpGoCmd :: LookupFn
httpGoCmd cmd = Just ("open http://go/" ++ (head cmd))

getRoutes lines = Map.fromList $ map getRoute lines
  where getRoute line = let (a, b) = (break (== ' ') line)
                        in (a, tail b)

readFileCmds :: String -> IO LookupFn
readFileCmds fn = do
  fileData <- readFile fn
  let rts = getRoutes (lines fileData)
  return (\x -> ((flip Map.lookup) rts) (head x))

-- | 'main' runs the main program
main :: IO ()
main = do
  args <- getArgs
  homeDir <- getEnv "HOME"
  fileCmd <- readFileCmds (homeDir ++ "/.go-routes")
  let resolvers = [fileCmd, httpGoCmd]
  -- This fold just finds the first Just or returns Nothing
  let cmdMaybe = foldl mplus Nothing $ map (\lu -> lu args) resolvers
  -- This errors out if nothing matches, but that should never happen
  putStrLn $ fromJust cmdMaybe
