{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DjambiSpec where

import           Test.Hspec

data Game = Game { plays :: [ Play ] }
  deriving (Eq, Show)

initialGame :: Game
initialGame = Game []

getBoard :: Game -> Board
getBoard = foldr apply initialBoard . plays

-- assumes Play is always valid wrt Board
-- implies apply is NOT total
apply :: Play -> Board -> Board
apply (Play (C, 1) to) (Board [ Militant Vert (C,1)]) = Board [ Militant Vert to ]
apply (Play (D, 1) to) (Board [ Militant Vert (D,1)]) = Board [ Militant Vert to ]

data Board = Board [ Piece ]
  deriving (Eq, Show)

initialBoard :: Board
initialBoard = Board [ Militant Vert (C,1) ]

data Piece = Militant Party Position
  deriving (Eq, Show)

data Party = Vert
  deriving (Eq, Show)

data Row = A | B | C | D | E | F | G | H | I
  deriving (Eq, Show)


newtype Col = Col Int
  deriving (Eq, Show, Num)

type Position = (Row, Col)

data Play = Play Position Position
  deriving (Eq, Show)

data DjambiError = InvalidPlay
  deriving (Eq, Show)

play :: Play -> Game -> Either DjambiError Game
play p (Game ps) = Right $ Game $ p:ps

-- Plan
--
--  1. setup basic types for (pure) game logic
--     - get current Game
--     - get possible plays :: Game -> [ Play ]
--     - apply a Play on a Game
--  2. scaffold HTTP server to serve JSON
--  3. provide HTML Content type
--
spec :: Spec
spec = describe "Djambi Game" $ do

  it "on GET /game returns state of the game as JSON" $
    -- client <--- game state + possible plays
    -- client ---> play
    -- client <--- game state + possible plays
    -- ...
    pendingWith "need server scaffolding"

  describe "Core Game Logic" $ do

    let validplay = Play (C, 1) (D, 1)

    it "returns initial board when there is no play" $
      getBoard initialGame  `shouldBe` initialBoard

    it "updates the game state when playing, given a valid play" $ do
      let updatedGame = Game [validplay]
      play validplay initialGame  `shouldBe` Right updatedGame

    it "rejects play if it is not valid" $
      play (Play (C,1) (D, 3)) initialGame  `shouldBe` Left InvalidPlay

    -- it "returns updated board when there is one play" $ do
    --   getBoard (play validplay initialGame) `shouldBe` Board [ Militant Vert (D, 1) ]
    --   getBoard (play (Play (C, 1) (D, 2)) initialGame) `shouldBe` Board [ Militant Vert (D, 2) ]

    -- it "returns updated board when there are 2 plays" $
    --   getBoard (play (Play (D,1) (D,2)) (play validplay initialGame)) `shouldBe` Board [ Militant Vert (D, 2) ]

