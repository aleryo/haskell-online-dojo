module Djambi.Server where

import           Data.Aeson
import           Djambi
import           Network.HTTP.Types (status200)
import           Network.Wai
import           Servant
import           Servant.Server

type DjambiApi = "game" :> Get '[JSON] Board
            :<|> "possible-moves" :> Get '[JSON] [Play]

djambiApi :: Proxy DjambiApi
djambiApi = Proxy

djambiServer :: Application
djambiServer = serve djambiApi server
  where
    server = handlerGame :<|> handlerMoves

    handlerGame = pure initialBoard
    handlerMoves = pure []

djambiApp :: IO Application
djambiApp = pure djambiServer
