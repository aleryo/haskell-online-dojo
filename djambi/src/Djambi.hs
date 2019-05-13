{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

module Djambi where

import           Control.Monad
import           Data.Aeson    (FromJSON, ToJSON)
import           Data.List     (insertBy, sort, unfoldr, (\\))
import           Data.Maybe
import           Data.Ord
import           GHC.Generics (Generic)

data Game = Game { plays :: [ Play ] }
  deriving (Eq, Show)

getNextPlayer :: Game -> Party
getNextPlayer (Game [])                    = Vert
getNextPlayer (Game (Play pty _ _ : _))    = nextParty pty
getNextPlayer (Game (Kill pty _ _ : _))    = pty
getNextPlayer (Game (PlaceDead pty _ : _)) = nextParty pty

nextParty :: Party -> Party
nextParty Jaune = Vert
nextParty pty = succ pty

initialGame :: Game
initialGame = Game []

getBoard :: Game -> Board
getBoard = getBoardFrom initialBoard

getBoardFrom :: Board -> Game -> Board
getBoardFrom board = foldr apply board . plays

-- assumes Play is always valid wrt Board
apply :: Play -> Board -> Board
apply ply board @ (Board ps) = 
  case ply of 
    (Play _ from to) -> Board $ movePiece from to <$> ps
    (Kill _ from to) -> BoardWithCadaverToReplace $ movePiece from to <$> filter ((/= to) . position) ps
    _ -> error $ "invalid play " <> show ply <> " on board " <> show board
apply (PlaceDead _ pos) (BoardWithCadaverToReplace ps) = 
    Board (placePiece (Dead $ DeadPiece pos) ps)
apply ply board = error $ "invalid play " <> show ply <> " on board " <> show board

movePiece :: Position -> Position -> Piece -> Piece
movePiece from to m@(Militant (LivePiece pty from'))
          | from == from' = militant pty to
          | otherwise     = m
movePiece from to m@(Dead (DeadPiece from'))
          | from == from' = Dead $ DeadPiece to
          | otherwise     = m

data Board = Board { livePieces :: [ Piece ] } 
           | BoardWithCadaverToReplace { livePieces :: [ Piece ] } 
  deriving (Eq, Show, Generic)

instance ToJSON Board
instance FromJSON Board

initialBoard :: Board
initialBoard = Board [ militant Vert (A, 1)
                     , militant Vert (A, 2)
                     , militant Vert (A, 3)
                     , militant Vert (B, 1)
                     , militant Vert (B, 2)
                     , militant Vert (B, 3)
                     , militant Vert (C, 1)
                     , militant Vert (C, 2)
                     , militant Vert (C, 3)
                     , militant Rouge (A, 7)
                     , militant Bleu (G, 7)
                     , militant Jaune (G, 2)
                     ]

emptyPositions :: [ Piece ] -> [ Position ]
emptyPositions pieces = [ (x,y)  | x <- [ A .. I ], y <- [ 1 .. 9 ] ] \\ fmap position pieces

placePiece :: Piece -> [ Piece ] -> [ Piece ]
placePiece piece pieces = 
  insertBy (comparing position) piece pieces 

isOccupied :: Board -> Position -> Bool
isOccupied b = isJust . pieceAt b

pieceAt :: Board -> Position -> Maybe Piece
pieceAt b p = case b of 
  Board pieces -> getPieceAt pieces
  BoardWithCadaverToReplace pieces ->  getPieceAt pieces
  where
    getPieceAt pieces = listToMaybe (filter (\ piece -> position piece == p) pieces)

partyAt :: Board -> Position -> Maybe (Either DeadPiece LivePiece) 
partyAt b p = do
  piece <- pieceAt b p
  case piece of
    Militant l -> Just $ Right l
    Dead d -> Just $ Left d

livePiecesFrom :: Party -> [Piece] -> [Position]
livePiecesFrom pty = catMaybes . fmap posAndParty
    where
      posAndParty (Militant (LivePiece pty' pos)) 
        | pty' == pty = Just pos
        | otherwise = Nothing
      posAndParty _ = Nothing
         
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

data Index = A | B | C | D | E | F | G | H | I
  deriving (Enum, Bounded, Eq, Ord, Show, Generic)

instance ToJSON Index
instance FromJSON Index

newtype Col = Col { col :: Index }
  deriving newtype (Enum, Bounded, Eq, Ord, Num, ToJSON, FromJSON)

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
          | PlaceDead Party Position
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

allPossibleMoves :: Board -> Game -> [Play]
allPossibleMoves initBoard game = 
  let b = getBoardFrom initBoard game
      party' = getNextPlayer game
        
  in case b of 
       (BoardWithCadaverToReplace pieces) -> 
        [ PlaceDead party' pos | pos <- emptyPositions pieces ]
       board @(Board ps) -> do
         p <- livePiecesFrom party' ps
         possibleMoves board party' p

possibleMoves :: Board -> Party -> Position -> [Play]
possibleMoves b pty from = sort (fmap mkPlay piece)
  where
    piece = join [ possibleMove b pty from dir 2 | dir <- enumFromTo minBound maxBound ]
    mkPlay to | isOccupied b to = Kill pty from to
              | otherwise = Play pty from to

data Direction = East | South | West | North
               | SE | SW | NE | NW
  deriving (Eq, Show, Enum, Bounded)

possibleMove ::  Board -> Party -> Position -> Direction -> Integer -> [ Position ]
possibleMove b myParty pos d steps = unfoldr (uncurry nextStep) (pos,steps)
  where nextStep _ 0 = Nothing
        nextStep p n = do
          targetPos <- moveOnePosition p d
          case partyAt b targetPos of
            Just (Right (LivePiece pty _)) | pty /= myParty -> Just (targetPos, (targetPos, 0))
            Just  _ -> Nothing
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
play p g@(Game ps) | p `elem` allPossibleMoves initialBoard g = Right $ Game $ p:ps
                   | otherwise = Left (InvalidPlay p)

