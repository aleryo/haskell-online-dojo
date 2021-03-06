module Djambi.GameSpec where

import           Control.Monad
import           Data.List                 (sort)
import           Djambi
import           Test.Hspec

-- Plan
--
--  1. [x] setup basic types for (pure) game logic
--     - get current Game
--     - get possible plays :: Game -> [ Play ]
--     - apply a Play on a Game
--     1.1 list possible plays
--       - use same type for rows and cols
--  2. [x] scaffold HTTP server to serve JSON
--  3. [x] handle logical errors
--  4. enrich game play
--     a'. [x] add one player to handle game turn
--     b. [x] kill a piece
--     c. handle necromobile 
--     a. more pieces: Necromobile, Journaliste, Provocateur, Assassin, Chef
--     c. end game
--  4. plug https://github.com/berewt/J2S to get an AI opponent
--  5. provide HTML Content type

validplay :: Play
validplay = Play Vert (C, 1) (D, 1)

invalidPlay :: Play
invalidPlay = Play Vert (C,1) (D, 3)

spec :: Spec
spec = describe "Djambi Game" $ do

  describe "Core Game Logic" $ do

    it "returns initial board when there is no play" $
      getBoard initialGame  `shouldBe` initialBoard

    it "updates the game state when playing, given a valid play" $ do
      let updatedGame = Game [validplay]
      play validplay initialGame  `shouldBe` Right updatedGame

    describe "Coordinates computation" $ do
      it "can compute abstract movement of a piece horizontally" $ do
          moveOnePosition (C, 1) East  `shouldBe` pure (C, 2)
          moveOnePosition (C, 2) East  `shouldBe` pure (C, 3)
          moveOnePosition (C, 2) East  `shouldBe` pure (C, 3)
          moveOnePosition (C, 9) East  `shouldBe` Nothing
          moveOnePosition (C, 9) West  `shouldBe` pure (C, 8)
          moveOnePosition (D, 7) West  `shouldBe` pure (D, 6)
          moveOnePosition (D, 7) East  `shouldBe` pure (D, 8)

      it "can compute abstract movement of a piece vertically" $ do
          moveOnePosition (C, 1) South  `shouldBe` pure (D, 1)
          moveOnePosition (I, 1) South  `shouldBe` Nothing
          moveOnePosition (C, 1) North  `shouldBe` pure (B, 1)
          moveOnePosition (A, 1) North `shouldBe` Nothing
          moveOnePosition (C, 3) North  `shouldBe` pure (B, 3)
          moveOnePosition (C, 3) South  `shouldBe` pure (D, 3)

      it "can compute abstract movement of a piece diagonally" $ do
          moveOnePosition (C, 1) SE  `shouldBe` pure (D, 2)
          moveOnePosition (C, 2) SW  `shouldBe` pure (D, 1)
          moveOnePosition (C, 1) NE  `shouldBe` pure (B, 2)
          moveOnePosition (C, 2) NW  `shouldBe` pure (B, 1)

    it "generates a list of possible plays for militant" $ do
      --  1 2 3
      -- A.   .
      -- B. .
      -- C+ . .
      -- D. .
      -- E.   .
      possibleMoves initialBoard Vert (C, 1)
        `shouldBe` sort [Play Vert (C, 1) p | p <- [(D, 1), (D, 2), (E, 1), (E, 3)]]

      possibleMoves initialBoard Vert (A, 3)
        `shouldBe` sort [Play Vert (A, 3) p | p <- [(A, 4), (B, 4), (A, 5), (C, 5)]]

      possibleMoves initialBoard Vert (B, 1) `shouldBe` []

    it "generates a list of all possible moves" $ do
      allPossibleMoves initialBoard initialGame `shouldContain` possibleMoves initialBoard Vert (C,1)
      allPossibleMoves initialBoard initialGame `shouldContain` possibleMoves initialBoard Vert (C,2)

    it "rejects play if it is not valid" $
      -- The game piece in C1 is a activist, so it can only move by
      -- one or two steps horizontally, vertically or diagonally,
      -- making this move invalid
      play invalidPlay initialGame  `shouldBe` Left (InvalidPlay invalidPlay)

    it "returns updated board when there are 2 plays" $
      getBoardFrom (Board [militant Vert (C,1), militant Rouge (A,7)]) <$> (play (Play Rouge (A, 7) (A, 6)) =<< play validplay initialGame)
        `shouldBe` Right (Board [ militant Vert (D,1), militant Rouge (A,6) ])

  describe "Next Player Logic" $ do

    it "gives Vert as next player when starting game" $
      getNextPlayer initialGame `shouldBe` Vert

    it "gives Rouge after a valid play" $
      getNextPlayer <$> play validplay initialGame `shouldBe` Right Rouge

    it "gives Bleu after a valid play by Rouge" $ do
      let twoPlays = do
            intermediate <- play validplay initialGame
            play (Play Rouge (A, 7) (A, 6)) intermediate
      getNextPlayer <$> twoPlays `shouldBe` Right Bleu

    it "gives Jaune after a valid play by Bleu" $ do
        let threePlays = foldM (flip play) initialGame [validplay, Play Rouge (A, 7) (A, 6), Play Bleu (G, 7) (F, 6)]
        getNextPlayer <$> threePlays `shouldBe` Right Jaune

    it "gives Vert after a valid play by Jaune" $ do
        let fourPlays = foldM (flip play) initialGame [validplay, Play Rouge (A, 7) (A, 6), Play Bleu (G, 7) (F, 6), Play Jaune (G, 2) (F, 2)]
        getNextPlayer <$> fourPlays `shouldBe` Right Vert
  
  describe "Dead Piece" $ do
    let fictitiousBoard = Board [ militant Vert (A, 1)
                                , militant Rouge (A, 3)
                                ] 
    let game = Game [ Kill Vert (A, 1) (A,3) ]

    it "killing a piece is a possible move" $ do
      possibleMoves fictitiousBoard Vert (A, 1)
        `shouldContain` [ Kill Vert (A, 1) (A,3) ]

    it "replace piece when applying Kill move" $ do
      getBoardFrom fictitiousBoard game
        `shouldBe` BoardWithCadaverToReplace [ militant Vert (A, 3) ] 

    it "possible moves require player to place killed piece" $ 
      allPossibleMoves fictitiousBoard game 
        `shouldBe` [ PlaceDead Vert (l,c) | l <- [A .. I], c <- [1 .. 9], (l,c) /= (A, 3)]

    -- TODO cannot place a dead in central case

    it "can replace dead adversary on an empty position in board " $ 
      getBoardFrom fictitiousBoard (Game  [ PlaceDead Vert (A,4),  Kill Vert (A, 1) (A,3) ])
        `shouldBe` Board [ militant Vert (A, 3), dead (A, 4) ]

    it "update next player after placing a dead " $ 
      getNextPlayer <$> play (PlaceDead Vert (A,4)) game `shouldBe` Right Rouge

    it "prevents movement"  $ do
      let boardWithOneDead = Board [ militant Vert (A, 1)
                                   , dead (A, 2)
                                   ] 
      possibleMoves boardWithOneDead Vert (A,1)
        `shouldBe` sort [Play Vert (A, 1) x | x <- [(B, 2), (C, 3), (B, 1), (C, 1)]]
