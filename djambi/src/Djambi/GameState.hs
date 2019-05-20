{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Djambi.GameState where

import Data.Aeson
import Data.Text
import Djambi.Game
import GHC.Generics

type Player = Text

data GameState = GameState { gameState :: Game, players :: [Player] }
  deriving (Generic, ToJSON)