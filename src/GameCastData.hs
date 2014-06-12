{-# LANGUAGE OverloadedStrings #-}
module GameCastData where
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text

{- High Level Data Structure -}
data GameCastData = ParseFailure | GameCastData
  { snapshotId :: Int
  , sessionId :: Text
  , current :: Current
  , plays :: [Play]
  } deriving Show

instance FromJSON GameCastData where
  parseJSON (Object v) = GameCastData <$> v .: "snapshotId"
                                      <*> v .: "sessionId"
                                      <*> v .: "current"
                                      <*> v .: "plays"
  parseJSON _          = mzero

{- Current Data -}
data Current = NoState | Current
  { balls :: Text
  , strikes :: Text
  , outs :: Text
  , onBase :: [Player]
  } deriving Show

instance FromJSON Current where
  parseJSON (Object v) = Current <$> v .: "balls"
                                 <*> v .: "strikes"
                                 <*> v .: "outs"
                                 <*> v .: "onBase"
  parseJSON _          = mzero

{- Player Data -}
data Player = Player
  { playerID :: Int
  , firstName :: Text
  , lastName :: Text
  } deriving Show

instance FromJSON Player where
  parseJSON (Object v) = Player <$> v .: "id"
                                <*> v .: "firstName"
                                <*> v .: "lastName"
  parseJSON _          = mzero

{- All the information for a single Play -}
data Play = Play
  { playID :: Int
  , atbatID :: Int
  , playResult :: Text
  , awayScore :: Int
  , homeScore :: Int
  , isResult :: Bool
  , inningText :: Text
  , pitcher :: Player
  , batter :: Player
  , pitchXCoord :: Int
  , pitchYCoord :: Int
  , velocity :: Text
  } deriving Show

{-- Have to do custom FromJSON because it has an 'id' field --}
instance FromJSON Play where
  parseJSON (Object v) = Play <$> v .: "id"
                              <*> v .: "atbatId"
                              <*> v .: "playResult"
                              <*> v .: "awayScore"
                              <*> v .: "homeScore"
                              <*> v .: "isResult"
                              <*> v .: "inningText"
                              <*> v .: "pitcher"
                              <*> v .: "batter"
                              <*> v .: "pitchXCoord"
                              <*> v .: "pitchYCoord"
                              <*> (v .:? "velocity" .!= "NO PITCH VELOCITY DATA")
  parseJSON _          = mzero
