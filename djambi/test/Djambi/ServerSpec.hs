module Djambi.ServerSpec where

import           Control.Monad
import           Data.Aeson                (ToJSON, encode)
import           Data.String               (fromString)
import           Data.Text.Lazy            (unpack)
import           Data.Text.Lazy.Encoding   (decodeUtf8)
import           Djambi
import           Djambi.Server
import           Network.HTTP.Types.Method
import           Test.Hspec
import           Test.Hspec.Wai            hiding (pendingWith)

validplay :: Play
validplay = Play Vert (C, 1) (D, 1)

invalidPlay :: Play
invalidPlay = Play Vert (C,1) (D, 3)

spec :: Spec
spec =
  with djambiApp $ describe "Djambi Server" $ do
    let json :: (ToJSON a) => a -> ResponseMatcher
        json = fromString . unpack . decodeUtf8 . encode

    it "on GET /game returns state of the game as JSON" $
      get "/game" `shouldRespondWith` json initialBoard

    it "on GET /possible-moves returns list of valid plays for current player as JSON" $
      get "/possible-moves" `shouldRespondWith` json (allPossibleMoves initialBoard initialGame)

    it "on POST /move returns updated board as JSON given move is legal" $ do
      let move = encode validplay
          Right updatedGame = play validplay initialGame
      request methodPost "/move" [("content-type", "application/json")] move `shouldRespondWith` json (getBoard updatedGame)

    it "on POST a second /move returns updated board" $ do
      let firstMove = encode validplay
          secondPlay = Play Rouge (A, 7) (A, 6)
          secondMove = encode secondPlay
          Right updatedGame = play validplay initialGame >>= play secondPlay
      void $ request methodPost "/move" [("content-type", "application/json")] firstMove
      request methodPost "/move" [("content-type", "application/json")] secondMove `shouldRespondWith` json (getBoard updatedGame)

    it "on POST /move returns error 400 given move is invalid" $ do
      let move = encode invalidPlay
      request methodPost "/move" [("content-type", "application/json")] move `shouldRespondWith` 400
