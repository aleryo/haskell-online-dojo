{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Api where

import Data.Aeson(ToJSON, FromJSON)
import Servant
import Intcode
import Servant.Server (serve)
import Data.Proxy (Proxy(..))
import Network.Wai (Application)

type Api = "intcode" :> ReqBody '[JSON] IntCodeProgram :> Post '[JSON] IntCodeProgram
 
newtype IntCodeProgram = IntCodeProgram [Int]
    deriving (ToJSON, FromJSON)

intcodeService :: IntCodeProgram -> IntCodeProgram
intcodeService (IntCodeProgram prog) = IntCodeProgram (interpret prog) 

intcodeServer :: Application
intcodeServer = serve (Proxy :: Proxy Api) server
    where
        server :: IntCodeProgram -> Handler IntCodeProgram
        server = pure . intcodeService

