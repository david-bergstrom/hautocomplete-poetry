import Data.List
import Data.String.Utils
import Data.Maybe
import Network.HTTP
import Text.Regex.Posix

--find_words text = fmap get_word (matchRegexAll (mkRegex ".") text)
--get_word (_, word, _, _) = word

get_words :: String -> [String]
get_words text = map head (text =~ ex)

extract_matches = (take 3) . (drop 1)

ex = "[a-z ]+"

getUrl query = "http://suggestqueries.google.com/complete/search?client=firefox&q=" ++ (escape query)
               where                 
                 escape = replace " " "%20"

main = do
  let query = "what would"
  rsp <- Network.HTTP.simpleHTTP (getRequest $ getUrl query)
         -- fetch document and return it (as a 'String'.)
  output <- getResponseBody rsp
  let pretty = ((intercalate "\n") . extract_matches . get_words) output
  putStrLn pretty
