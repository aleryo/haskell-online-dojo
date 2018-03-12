{-# LANGUAGE OverloadedStrings #-}
module Sql.DB.VectorSpec where

import           Control.Monad.State
import           Data.Monoid         ((<>))
import           Data.Text
import qualified Data.Vector         as Vector
import           Sql.DB
import           Sql.DB.Vector
import           Test.Hspec


spec :: Spec
spec = describe "Binary Representation of DB" $ do

  it "initialisez empty vector" $ do
    bytes initDB `shouldBe` Vector.empty
