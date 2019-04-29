{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

module Djambi where

import           Control.Monad
import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Function (on)
import           Data.List     (sort, unfoldr)
import           Data.Maybe
import           Data.Ord      (comparing)
import           GHC.Generics

data Game = Game { plays :: [ Play ] }
  deriving (Eq, Show)

getNextPlayer :: Game -> Party
getNextPlayer (Game [])                   = Vert
getNextPlayer (Game (Play Jaune _ _ : _)) = Vert
getNextPlayer (Game (Play play _ _ : _))  = succ play

initialGame :: Game
initialGame = Game []

getBoard :: Game -> Board
getBoard = getBoardFrom initialBoard

getBoardFrom :: Board -> Game -> Board
getBoardFrom board = foldr apply board . plays

-- assumes Play is always valid wrt Board
apply :: Play -> Board -> Board
apply (Play _ from to) (Board ps) = Board $ movePiece <$> ps
  where
    movePiece m@(Militant party from')
          | from == from' = Militant party to
          | otherwise     = m

data Board = Board [ Piece ]
  deriving (Eq, Show, Generic)

instance ToJSON Board
instance FromJSON Board

initialBoard :: Board
initialBoard = Board [ Militant Vert (A, 1)
                     , Militant Vert (A, 2)
                     , Militant Vert (A, 3)
                     , Militant Vert (B, 1)
                     , Militant Vert (B, 2)
                     , Militant Vert (B, 3)
                     , Militant Vert (C, 1)
                     , Militant Vert (C, 2)
                     , Militant Vert (C, 3)
                     , Militant Rouge (A, 7)
                     , Militant Bleu (G, 7)
                     , Militant Jaune (G, 2)
                     ]

isOccupied :: Board -> Position -> Bool
isOccupied (Board pieces) p = p `elem` fmap position pieces

pieceAt :: Board -> Position -> Maybe Piece
pieceAt (Board pieces) p = listToMaybe (filter (\ piece -> position piece == p) pieces)

data Piece = Militant { party :: Party, position :: Position }
  deriving (Eq, Show, Generic)

instance ToJSON Piece
instance FromJSON Piece

data Party = Vert | Rouge | Bleu | Jaune
  deriving (Eq, Enum, Ord, Show, Generic)

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
          | Kill Party Position Position    
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
  let party' = getNextPlayer game
  Militant _ p <- filter ((== party') . party) ps
  possibleMoves b party' p

possibleMoves :: Board -> Party -> Position -> [Play]
possibleMoves b party from@(x, y) = sort (fmap mkPlay militant)
  where
    militant = join [ possibleMove b party from dir 2 | dir <- enumFromTo minBound maxBound ]
    mkPlay to | isOccupied b to = Kill party from to
              | otherwise = Play party from to

data Direction = East | South | West | North
               | SE | SW | NE | NW
  deriving (Eq, Show, Enum, Bounded)

possibleMove ::  Board -> Party -> Position -> Direction -> Integer -> [ Position ]
possibleMove b myParty position d steps = unfoldr (uncurry nextStep) (position,steps)
  where nextStep _ 0 = Nothing
        nextStep p n = do
          targetPos <- moveOnePosition p d
          case party <$> pieceAt b targetPos of
            Just pty | pty /= myParty -> Just (targetPos, (targetPos, 0))
            Just pty -> Nothing
            Nothing -> pure (targetPos, (targetPos, n-1))

moveOnePosition :: Position -> Direction -> Maybe Position
moveOnePosition (l, c) East  = (\c' -> (l, c')) <$> safeShift c 1
moveOnePosition (l, c) West  = (\c' -> (l, c')) <$> safeShift c (- 1)
moveOnePosition (l, c) South = (\l' -> (l', c)) <$> safeShift l 1
moveOnePosition (l, c) North = (\l' -> (l', c)) <$> safeShift l (- 1)
moveOnePosition p      SE    = foldM (\pos dir -> moveOnePosition pos dir) p [East, South]
moveOnePosition p      SW    = foldM (\pos dir -> moveOnePosition pos dir) p [West, South]
moveOnePosition p      NE    = foldM (\pos dir -> moveOnePosition pos dir) p [East, North]
moveOnePosition p      NW    = foldM (\pos dir -> moveOnePosition pos dir) p [West, North]

play :: Play -> Game -> Either DjambiError Game
play p@(Play _ from to) g@(Game ps) | p `elem` allPossibleMoves g = Right $ Game $ p:ps
                                    | otherwise = Left (InvalidPlay p)
