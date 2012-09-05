import System.Environment
import qualified Data.Map as Map
import Data.Maybe
 
-- | 'main' runs the main program
main :: IO ()
main = do
  homeDir <- getEnv "HOME"
  fileData <- readFile (homeDir ++ "/.go-routes")
  let rts = getRoutes (lines fileData)
  (route:rest) <- getArgs
  putStrLn $ bashCmd rts route

getRoute :: String -> (String, String)
getRoute line
  = let (a, b) = (break (== ' ') line)
    in (a, tail b)

getRoutes lines = Map.fromList $ map getRoute lines

bashCmd :: Map.Map String String -> String -> String
bashCmd routes cmd = fromMaybe "" $ Map.lookup cmd routes
