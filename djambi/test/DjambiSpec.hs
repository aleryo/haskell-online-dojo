{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DjambiSpec where

import           Test.Hspec

main :: IO ()
main = putStrLn "Test suite not yet implemented"

data Game = Game [ Play ]
  deriving (Eq, Show)

initialGame :: Game
initialGame = Game []

getBoard :: Game -> Board
getBoard = const initialBoard

data Board = Board [ Piece ]
  deriving (Eq, Show)

initialBoard :: Board
initialBoard = Board []

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

play :: Play -> Game -> Game
play p (Game ps) = Game $ p:ps

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
      play validplay initialGame  `shouldBe` updatedGame

    it "returns updated board when there is one play" $
      getBoard (play validplay initialGame) `shouldBe` Board [ Militant Vert (D, 1) ]

