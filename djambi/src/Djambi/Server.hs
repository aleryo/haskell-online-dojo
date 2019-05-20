module Djambi.Server where

import           Control.Monad.Trans (liftIO)
import           Data.IORef
import           Djambi
import           Djambi.GameState
import           Network.Wai
import           Servant

type DjambiApi = "game" :> Get '[JSON] Board
            :<|> "possible-moves" :> Get '[JSON] [Play]
            :<|> "move" :> ReqBody '[JSON] Play :> Post '[JSON] Board
            :<|> "games" :> Post '[JSON] GameState

djambiApi :: Proxy DjambiApi
djambiApi = Proxy

djambiServer :: IORef GameState -> Application
djambiServer gameStateRef = serve djambiApi server
  where
    server = handlerGame 
        :<|> handlerMoves
        :<|> handlerMove
        :<|> handlerNewGame

    handlerGame = liftIO $ getBoard . gameState <$> readIORef gameStateRef

    handlerMoves = liftIO $ allPossibleMoves initialBoard . gameState <$> readIORef gameStateRef

    handlerNewGame = liftIO $ readIORef gameStateRef

    -- TODO handle race conditions...
    handlerMove move = do
      gs @ (GameState game _) <- liftIO $ readIORef gameStateRef
      let result = play move game
          updateGameAndReturn game' = liftIO $ do
            writeIORef gameStateRef gs{gameState = game'}
            pure $ getBoard game'
      either (const $ throwError err400) updateGameAndReturn result

djambiApp :: IO Application
djambiApp = newIORef (GameState initialGame []) >>= pure . djambiServer
