{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Djambi where

import           Control.Monad
import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Functor
import           Data.List     (sort)
import           Data.Maybe
import           GHC.Generics

data Game = Game { plays          :: [ Play ] }
  deriving (Eq, Show)

getNextPlayer :: Game -> Party
getNextPlayer (Game []) = Vert
getNextPlayer _         = Rouge

initialGame :: Game
initialGame = Game []

getBoard :: Game -> Board
getBoard = foldr apply initialBoard . plays

-- assumes Play is always valid wrt Board
apply :: Play -> Board -> Board
apply (Play _ from to) (Board ps)
  = Board (movePiece <$> ps)
  where
    movePiece m@(Militant party from')
          | from == from' = Militant party to
          | otherwise     = m

data Board = Board [ Piece ]
  deriving (Eq, Show, Generic)

instance ToJSON Board
instance FromJSON Board

initialBoard :: Board
initialBoard = Board [ Militant Vert (C, 1), Militant Rouge (A, 7) ]

data Piece = Militant Party Position
  deriving (Eq, Show, Generic)

instance ToJSON Piece
instance FromJSON Piece

data Party = Vert | Rouge
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Party
instance FromJSON Party

data Index = A | B | C | D | E | F | G | H | I
  deriving (Enum, Bounded, Eq, Ord, Show, Generic)

instance ToJSON Index
instance FromJSON Index

newtype Col = Col { col :: Index }
  deriving (Enum, Bounded, Eq, Ord, Num)

deriving newtype instance ToJSON Col
deriving newtype instance FromJSON Col

type Row = Index

instance Show Col where
  show c = case col c of
    A -> "1"
    B -> "2"
    C -> "3"
    D -> "4"
    E -> "5"
    F -> "6"
    G -> "7"
    H -> "8"
    I -> "9"

instance Num Index where
  fromInteger 1 = A
  fromInteger 2 = B
  fromInteger 3 = C
  fromInteger 4 = D
  fromInteger 5 = E
  fromInteger 6 = F
  fromInteger 7 = G
  fromInteger 8 = H
  fromInteger 9 = I
  fromInteger _ = error "out of bounds"

  (+)    _ _ = error "Unsupported"
  (*)    _ _ = error "Unsupported"
  (-)    _ _ = error "Unsupported"
  signum _   = error "Unsupported"
  abs    _   = error "Unsupported"

type Position = (Row, Col)

data Play = Play Party Position Position
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Play
instance FromJSON Play

data DjambiError = InvalidPlay Play
  deriving (Eq, Show)

safeShift :: (Bounded a, Ord a, Enum a) => a -> Integer -> Maybe a
safeShift value shift
  | shift == 0 = pure value
  | shift >  0 = guard (value /= maxBound) *> safeShift (succ value) (pred shift)
  | otherwise  = guard (value /= minBound) *> safeShift (pred value) (succ shift)

allPossibleMoves :: Game -> [Play]
allPossibleMoves game = do
  let b@(Board ps) = getBoard game
  let party = getNextPlayer game
  Militant _ p <- filter (\ (Militant party' _) -> party == party') ps
  possibleMoves b party p

possibleMoves :: Board -> Party -> Position -> [Play]
possibleMoves b party from@(x, y) = sort [Play party from to | to <- militant]
  where
    militant = catMaybes [ possibleMove from dir n | dir <- enumFromTo minBound maxBound, n <- [1,2] ]

data Direction = East | South | West | North
               | SE | SW | NE | NW
  deriving (Eq, Show, Enum, Bounded)

possibleMove :: Position -> Direction -> Integer -> Maybe Position
possibleMove (l, c) East n  = (\c' -> (l, c')) <$> safeShift c n
possibleMove (l, c) West n  = (\c' -> (l, c')) <$> safeShift c (- n)
possibleMove (l, c) South n = (\l' -> (l', c)) <$> safeShift l n
possibleMove (l, c) North n = (\l' -> (l', c)) <$> safeShift l (- n)
possibleMove p      SE n    = foldM (\pos dir -> possibleMove pos dir n) p [East, South]
possibleMove p      SW n    = foldM (\pos dir -> possibleMove pos dir n) p [West, South]
possibleMove p      NE n    = foldM (\pos dir -> possibleMove pos dir n) p [East, North]
possibleMove p      NW n    = foldM (\pos dir -> possibleMove pos dir n) p [West, North]

play :: Play -> Game -> Either DjambiError Game
play p@(Play _ from to) g@(Game ps) | p `elem` allPossibleMoves g = Right $ Game $ p:ps
                                    | otherwise = Left (InvalidPlay p)
