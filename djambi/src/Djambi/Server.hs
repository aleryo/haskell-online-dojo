module Djambi.Server where

import           Data.Aeson
import           Djambi
import           Network.HTTP.Types (status200)
import           Network.Wai

djambiServer :: Application
djambiServer req respond =
  respond $ responseLBS status200 [] (encode initialBoard)

djambiApp :: IO Application
djambiApp = pure djambiServer
