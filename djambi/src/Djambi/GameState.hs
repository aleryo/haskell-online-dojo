{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Djambi.GameState where

import Data.Aeson
import Djambi.Game
import GHC.Generics

data GameState = GameState { gameState :: Game, players :: Int }
  deriving (Generic, ToJSON)