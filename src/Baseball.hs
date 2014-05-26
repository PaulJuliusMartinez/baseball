{-# LANGUAGE OverloadedStrings #-}
module Baseball where
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text
import System.Environment
import Network.Curl.Download as Curl
import Text.Parsec
import Text.Parsec.ByteString.Lazy

main :: IO ()
main = do site <- liftM (!! 0) getArgs
          printRecentPlays site 0

printRecentPlays :: String -> Int -> IO ()
printRecentPlays url latest = do
  recentPlays <- getRecentPlays url latest
  mapM_ (\p -> putStrLn $ unpack $ playResult p) (Prelude.reverse recentPlays)
  let newLatest = case recentPlays of
                    [] -> latest
                    (x:_) -> pID x
  threadDelay 30000000
  printRecentPlays url newLatest

getRecentPlays :: String -> Int -> IO [Play]
getRecentPlays url latest = do
  putStrLn "FETCHING DATA..."
  webData <- getWebData url
  let parsedData = parseGameData webData
  case parsedData of
    ParseFailure -> return []
    gcdData -> return $ Prelude.takeWhile ((> latest) . pID) (plays gcdData)

{- Fetch the Gamecast source HTML, then find the JSON of the game data
 - and returns it as a single string. -}

getWebData :: String -> IO C.ByteString
getWebData site = do result <- Curl.openURI site
                     case result of
                       (Left str) -> return $ C.pack ("Error: " ++ str)
                       (Right bytes) -> return $ getGameData (C.fromStrict bytes)

getGameData :: C.ByteString -> C.ByteString
getGameData input = case parse findGameData "espn" input of
  Left err -> C.pack ("Error getting game data: " ++ show err)
  Right jsonData -> jsonData

findGameData :: Parser C.ByteString
findGameData = do _ <- manyTill anyChar $ try $ string "\"gamecast\":"
                  jsonData <- manyTill anyChar $ try $ (string "}," >> spaces >> string "'sport'")
                  return $ C.pack jsonData

{- Get useful data from the giant JSON object. -}

parseGameData :: C.ByteString -> GameCastData
parseGameData input = case decode input of
                        Nothing -> ParseFailure
                        (Just gameData) -> gameData

data GameCastData = ParseFailure | GameCastData
  { snapshotId :: Int
  , sessionId :: Text
  , plays :: [Play]
  } deriving Show

data Play = Play
  { pID :: Int
  , atbatID :: Int
  , playResult :: Text
  , balls :: Int
  , strikes :: Int
  , outs :: Int
  , isResult :: Bool
  , inningText :: Text
  -- , pitcher :: PlayerRef -- <- These are maybes!
  -- , batter :: PlayerRef -- <- These are maybes!
  , pitchXCoord :: Int
  , pitchYCoord :: Int
  , velocity :: Text
  } deriving Show

instance FromJSON GameCastData where
  parseJSON (Object v) = GameCastData <$> v .: "snapshotId"
                                      <*> v .: "sessionId"
                                      <*> v .: "plays"
  parseJSON _          = mzero

{-- Have to do custom FromJSON because it has an 'id' field --}
instance FromJSON Play where
  parseJSON (Object v) = Play <$> v .: "id"
                              <*> v .: "atbatId"
                              <*> v .: "playResult"
                              <*> v .: "balls"
                              <*> v .: "strikes"
                              <*> v .: "outs"
                              <*> v .: "isResult"
                              <*> v .: "inningText"
                              -- <*> v .: "pitcher"
                              -- <*> v .: "batter"
                              <*> v .: "pitchXCoord"
                              <*> v .: "pitchYCoord"
                              <*> (v .:? "velocity" .!= "NO PITCH VELOCITY DATA")
  parseJSON _          = mzero
