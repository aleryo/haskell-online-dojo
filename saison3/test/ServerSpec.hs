{-# LANGUAGE OverloadedStrings #-}
module ServerSpec
where

import Test.Hspec
import Test.Hspec.Wai -- https://hackage.haskell.org/package/hspec-wai
import Test.Hspec.Wai.JSON
--import Network.Wai.Test 
import Data.ByteString(ByteString)
import Network.Wai
import Api

-- Middleware :: Application -> Application
-- Application :: Request -> (Response -> IO ResponseResult) -> IO ResponseResult
app :: IO Application
app = pure intcodeServer

spec :: Spec
spec = with app $ describe "" $ do

    it "on POST interprets a given program" $ do
        postJSON "/intcode" "[2,3,0,3,2,3,0,3,99]" `shouldRespondWith` "[2,3,0,12,2,3,0,3,99]"


postJSON route payload = request "POST" route [("Content-Type", "application/json")] payload