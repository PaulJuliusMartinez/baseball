module Baseball where
import Control.Monad
import Data.Aeson
import System.Environment
import Network.Curl.Download as Curl
import Text.ParserCombinators.Parsec

main :: IO ()
main = do site <- liftM (!! 0) getArgs
          putStrLn $ getWebData site

{- Fetch the Gamecast source HTML, then find the JSON of the game data
 - and returns it as a single string. -}

getWebData :: String -> IO String
getWebData site = do result <- Curl.openURIString site
                     case result of
                       (Left str) -> return $ "Error: " ++ str
                       (Right bytes) -> return $ getGameData bytes

getGameData :: String -> String
getGameData input = case parse findGameData "espn" input of
  Left err -> "Error getting game data: " ++ show err
  Right json -> json

findGameData :: Parser String
findGameData = do _ <- manyTill anyChar $ try $ string "var settings = "
                  json <- manyTill anyChar $ try $ string "</script>"
                  return json

