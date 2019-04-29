module Djambi.Server where

import           Control.Monad.Trans (liftIO)
import           Data.IORef
import           Djambi
import           Network.Wai
import           Servant

type DjambiApi = "game" :> Get '[JSON] Board
            :<|> "possible-moves" :> Get '[JSON] [Play]
            :<|> "move" :> ReqBody '[JSON] Play :> Post '[JSON] Board

djambiApi :: Proxy DjambiApi
djambiApi = Proxy

djambiServer :: IORef Game -> Application
djambiServer gameRef = serve djambiApi server
  where
    server = handlerGame :<|> handlerMoves :<|> handlerMove

    handlerGame = liftIO $ getBoard <$> readIORef gameRef

    handlerMoves = liftIO $ allPossibleMoves <$> readIORef gameRef

    -- TODO handle race conditions...
    handlerMove move = do
      game <- liftIO $ readIORef gameRef
      let result = play move game
          updateGameAndReturn game' = liftIO $ do
            writeIORef gameRef game'
            pure $ getBoard game'
      either (const $ throwError err400) updateGameAndReturn result

djambiApp :: IO Application
djambiApp = newIORef initialGame >>= pure . djambiServer
