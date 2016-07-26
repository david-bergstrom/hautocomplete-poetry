import Data.List
import Data.String.Utils
import Data.Maybe
import Network.HTTP
import Text.Regex.Posix

-- The data recevied from Google is has the following format:
-- ["query string", ["first suggestion", "second suggestion"]]
-- getWords extracts a list of suggestions
getWords :: String -> [String]
getWords text = map head (text =~ expression)
  where
    expression = "[a-z ]+"

-- The first word is the query itself
extractSuggestions :: String -> [String]
extractSuggestions = (drop 1) . getWords

getUrl :: String -> String
getUrl query = "http://suggestqueries.google.com/complete/search?client=firefox&q=" ++ (escape query)
               where                 
                 escape = replace " " "%20"

getSuggestions :: String -> IO [String]
getSuggestions query = do
  rsp <- Network.HTTP.simpleHTTP (getRequest $ getUrl query)
  body <- getResponseBody rsp
  return (extractSuggestions body)

printSuggestions String -> IO ()
printSuggestions line = do
  suggestions <- getSuggestions line
  putStrLn $ "  " ++ (intercalate "\n  " $ take 3 suggestions)

main = do
  putStr "Enter query: "
  line <- getLine
  putStrLn "Fetching top three suggestions from Google:"
  printSuggestions line
