{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving         #-}
module Djambi.Piece where

import Data.Aeson
import GHC.Generics
import Djambi.Position

data LivePiece = LivePiece { party :: Party, livePos :: Position }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype DeadPiece = DeadPiece { deadPos :: Position }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

position :: Piece -> Position
position (Militant (LivePiece _ pos)) = pos
position (Dead (DeadPiece pos)) = pos

data Piece = Militant LivePiece
        |  Dead DeadPiece
  deriving (Eq, Show, Generic)

militant :: Party -> Position -> Piece
militant pty pos = Militant (LivePiece pty pos)

dead :: Position -> Piece
dead = Dead . DeadPiece 

instance ToJSON Piece
instance FromJSON Piece

data Party = Vert | Rouge | Bleu | Jaune
  deriving (Eq, Enum, Ord, Show, Generic)

instance ToJSON Party
instance FromJSON Party

data Color = Live Party | NotLive

colorOf :: Piece -> Color
colorOf (Militant (LivePiece pty _)) = Live pty
colorOf Dead{} = NotLive
