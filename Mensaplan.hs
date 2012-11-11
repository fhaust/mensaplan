
-- some includes

import Network.HTTP
import Network.URI

import Text.HTML.TagSoup 

import Data.Dates hiding (Day)
import Data.List

import Text.Printf

import qualified Codec.Binary.UTF8.String as UTF8

-----------------------------

-- some datatypes used for clarity

data Menu = Menu {
  name :: String,
  desc :: String,
  price :: String 
} deriving (Eq) 

instance Show Menu where
  show (Menu n d _) = printf "%s: %s" n d 

data Day = Day {
  date :: String,
  menus :: [Menu]
}

instance Show Day where
  show (Day d ms) = printf "%s:\n\n%s" d ms' 
    where ms' = intercalate "\n" $ map show ms 

-----------------------------

-- | read in a page from an url and decode it from utf8
-- | (no idea why simpleHTTP doesn't do this)
openURL ::  String -> IO String
openURL url = fmap UTF8.decodeString pageSource 
  where (Just uri) = parseURI url 
        request    = mkRequest GET uri
        pageSource = getResponseBody =<< simpleHTTP request

uniMensaURL = "http://www.studentenwerkbielefeld.de/index.php?id=61" 
fhMensaURL  = "http://www.studentenwerkbielefeld.de/index.php?id=63" 

-----------------------------

-- the actual page parsing (effectivly only 14 lines and that is without much magic)

findDayBlocks ::  [Tag String] -> [[Tag String]]
findDayBlocks = partitions (~== "<div class=day-block>") 

parseDayInfo :: [Tag String] -> String 
parseDayInfo d = fromTagText $ sections (~== "<a class=day-information>") d !! 0 !! 1

parseDayMenues ::  [Tag String] -> [Menu]
parseDayMenues d = map parseMenuData $ partitions (~== "<tr>") d

parseMenuData ::  [Tag String] -> Menu
parseMenuData m = Menu n d p
  where columns   = partitions (\t -> (t ~== "<th>") || (t ~== "<td>" )) m
        [n, d, p] = map (unwords . words . innerText) columns 

parseMenues ::  [Tag String] -> [Day]
parseMenues tags = zipWith Day n m 
  where b = findDayBlocks tags
        n = map parseDayInfo   b 
        m = map parseDayMenues b 
  
getMenues :: String -> IO [Day]
getMenues url = do
  src <- openURL url 
  let tags = parseTags src
  return $ parseMenues tags 

-----------------------------

-- | get the current day as a number (Monday -> 1)
dayOfWeek :: IO Int
dayOfWeek = fmap (weekdayNumber . dateWeekDay) getCurrentDateTime 

-----------------------------

-- | plug it all together
main ::  IO ()
main = do
 
  d <- dayOfWeek

  if d > 5 
    then error "What are you doing here? It's weekend!"
    else do
      uniMenues <- getMenues uniMensaURL
      fhMenues  <- getMenues fhMensaURL
     
      putStrLn "\n --- Uni Plan --- "
      print $ uniMenues !! (d - 1)
    
      putStrLn "\n --- FH Plan --- "
      print $ fhMenues !! (d - 1)
