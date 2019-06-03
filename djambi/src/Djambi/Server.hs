module Djambi.Server where

import           Control.Monad.Trans (liftIO)
import           Data.IORef
import           Djambi
import           Djambi.GameState
import           Djambi.User
import           Network.Wai
import           Servant

type DjambiApi = "game" :> Get '[JSON] Board
            :<|> "game" :> Post '[JSON] User
            :<|> "possible-moves" :> Get '[JSON] [Play]
            :<|> "move" :> ReqBody '[JSON] Play :> Post '[JSON] Board
            :<|> "games" :> Post '[JSON] GameState

djambiApi :: Proxy DjambiApi
djambiApi = Proxy

djambiServer :: IORef GameState -> Application
djambiServer gameStateRef = serve djambiApi server
  where
    server = handlerGame
        :<|> handlerJoinUser 
        :<|> handlerMoves
        :<|> handlerMove
        :<|> handlerNewGame

    handlerGame = liftIO $ getBoard . gameState <$> readIORef gameStateRef
    
    getNextUser :: GameState -> Maybe User
    getNextUser (GameState _ numPlayers) 
       | numPlayers > fromEnum (maxBound :: Party) = Nothing
       | otherwise = Just $ User (toEnum numPlayers)

    addUser (GameState g n) = GameState g (n + 1)

    handlerJoinUser = do
       nextAvailablePlayer <- liftIO $ getNextUser <$> readIORef gameStateRef
       case nextAvailablePlayer of
         Just u -> liftIO (modifyIORef gameStateRef addUser) >> pure u
         Nothing -> throwError err400

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
djambiApp = newIORef (GameState initialGame 0) >>= pure . djambiServer
