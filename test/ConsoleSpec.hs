{-# LANGUAGE OverloadedStrings #-}
module ConsoleSpec where

import           Interpreter
import           Test.Hspec

spec :: Spec
spec = describe "SQL Mini Interpreter" $ do

  it "interprets '.exit' as Exit command" $ do
    interpret ".exit" `shouldBe` Exit

  it "interprets 'SELECT 42' as a SELECT statement" $ do
    interpret "SELECT 42" `shouldBe` Sql (Select (Number 42))

  it "interprets unknown string  as Unknown command" $ do
    interpret "foo" `shouldBe` Unknown "foo"


