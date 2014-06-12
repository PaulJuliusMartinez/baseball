{-# LANGUAGE OverloadedStrings #-}
module Baseball where
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text
import Network.Curl.Download as Curl
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import System.Environment

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Popup
import GameCastData

import Control.Concurrent.MVar
main :: IO ()
main = do
  exit <- newEmptyMVar
  _ <- initGUI
  Just xml <- xmlNew "res/atbat.glade"
  _ <- forkOS mainGUI
  _ <- forkIO $ do
    site <- liftM (!! 0) getArgs
    printRecentPlays site 0 xml
    putMVar exit "ExitSuccess"

  _ <- takeMVar exit
  return ()

printRecentPlays :: String -> Int -> GladeXML -> IO ()
printRecentPlays url latest xml = do
  (state, recentPlays) <- getRecentPlays url latest
  -- mapM_ (\p -> putStrLn $ unpack $ playResult p) (Prelude.reverse recentPlays)
  createAtBatWindow state recentPlays xml
  let newLatest = case recentPlays of
                    [] -> latest
                    (x:_) -> atbatID x
  putStrLn $ "Updated up to " ++ (show newLatest)
  threadDelay 20000000
  printRecentPlays url newLatest xml

getRecentPlays :: String -> Int -> IO (Current, [Play])
getRecentPlays url latest = do
  putStrLn "FETCHING DATA..."
  webData <- getWebData url
  let parsedData = parseGameData webData
  case parsedData of
    ParseFailure -> return (NoState, [])
    gcdData -> do
      let mostRecentPlays = Prelude.takeWhile ((>= latest) . atbatID) (plays gcdData)
      if Prelude.null mostRecentPlays
        then return (current gcdData, [])
        else do
          let mostRecent = atbatID (mostRecentPlays !! 0)
          return $ (current gcdData, Prelude.takeWhile ((== mostRecent) . atbatID) mostRecentPlays)

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
