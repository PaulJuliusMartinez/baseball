{-# LANGUAGE OverloadedStrings #-}
module Popup where
import Control.Concurrent
import Control.Monad
import Data.Text
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import System.Directory
import System.Cmd

import GameCastData

popupTime :: Int
popupTime = 5000000

xmlFile :: IO GladeXML
xmlFile = do
  Just xml <- xmlNew "res/atbat.glade"
  return xml

createAtBatWindow :: Current -> [Play] -> GladeXML -> IO ()
createAtBatWindow state plays xml = do
  _ <- forkIO (createAtBatWindowAsync state plays xml)
  return ()

createAtBatWindowAsync :: Current -> [Play] -> GladeXML -> IO ()
createAtBatWindowAsync state [] _ = putStrLn "No new data"
createAtBatWindowAsync state plays xml = do
  let aPlay = Prelude.last plays

  let pitcherData = pitcher aPlay
  let batterData = batter aPlay
  let pitcherName = playerName pitcherData
  let batterName = playerName batterData

  -- Set pitcher name/image
  unless (playerID pitcherData == 0) $ do
    pitcher <- xmlGetWidget xml castToLabel "pitcher-name"
    set pitcher [ labelText := pitcherName ]
    pitcherImg <- xmlGetWidget xml castToImage "pitcher-img"
    getPlayerPicture pitcherImg $ playerID pitcherData

  -- Set batter name/image
  unless (playerID batterData == 0) $ do
    batter <- xmlGetWidget xml castToLabel "batter-name"
    set batter [ labelText := batterName ]
    batterImg <- xmlGetWidget xml castToImage "batter-img"
    getPlayerPicture batterImg $ playerID batterData

  -- Place at bat description.
  let atBatText = toNewLineList (Prelude.reverse plays)
  text <- xmlGetWidget xml castToLabel "text"
  set text [ labelText := atBatText ]

  -- Score text
  let scoreText = (unpack $ inningText aPlay) ++ ": " ++
                  (show $ awayScore aPlay) ++ " - " ++
                  (show $ homeScore aPlay)
  score <- xmlGetWidget xml castToLabel "score"
  set score [ labelText := scoreText ]

  -- On base text
  first <- xmlGetWidget xml castToLabel "first-base"
  set first [ labelText := "On first: " ++ (playerName ((onBase state) !! 0)) ]
  second <- xmlGetWidget xml castToLabel "second-base"
  set second [ labelText := "On second: " ++ (playerName ((onBase state) !! 1)) ]
  third <- xmlGetWidget xml castToLabel "third-base"
  set third [ labelText := "On third: " ++ (playerName ((onBase state) !! 2)) ]

  -- Count
  let countText = (unpack $ balls state) ++ " - " ++
                  (unpack $ strikes state) ++ ", " ++
                  (unpack $ outs state) ++ " out(s)"
  count <- xmlGetWidget xml castToLabel "count"
  set count [ labelText := countText ]

  -- Display window
  window <- xmlGetWidget xml castToWindow "window"
  widgetShowAll window

  -- Wait 7 seconds
  threadDelay popupTime
  widgetHideAll window

{- Get the image data for player given their ID. -}
getPlayerPicture :: Image -> Int -> IO ()
getPlayerPicture img theirID = do
  let imgFile = "res/pics/" ++ (show theirID) ++ ".png"
  let imgURL = "http://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/"
              ++ (show theirID)
              ++ ".png&w=120&h=90&transparent=true"
  exists <- doesFileExist imgFile
  if exists
    then imageSetFromFile img imgFile
    else do
      _ <- system $ "curl \"" ++ imgURL ++ "\" > " ++ imgFile
      imageSetFromFile img imgFile

playerName :: Player -> String
playerName (Player 0 _ _) = "Nobody"
playerName (Player _ f l) = (unpack f) ++ " " ++ (unpack l)

toNewLineList :: [Play] -> String
toNewLineList [p] = (unpack $ playResult p)
toNewLineList (p:ps) = (unpack $ playResult p) ++ "\n" ++ (toNewLineList ps)
