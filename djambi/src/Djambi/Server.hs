module Djambi.Server where

import           Data.Aeson
import           Djambi
import           Network.HTTP.Types (status200)
import           Network.Wai
import           Servant
import           Servant.Server

type DjambiApi = "game" :> Get '[JSON] Board
            :<|> "possible-moves" :> Get '[JSON] [Play]
            :<|> "move" :> ReqBody '[JSON] Play :> Post '[JSON] Board

djambiApi :: Proxy DjambiApi
djambiApi = Proxy

djambiServer :: Application
djambiServer = serve djambiApi server
  where
    server = handlerGame :<|> handlerMoves :<|> handlerMove

    handlerGame = pure initialBoard
    handlerMoves = pure (allPossibleMoves initialBoard)
    handlerMove move = do
      let result = play move initialGame
      either (const $ throwError err500) (pure . getBoard) result

djambiApp :: IO Application
djambiApp = pure djambiServer
