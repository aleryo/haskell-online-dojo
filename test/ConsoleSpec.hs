{-# LANGUAGE OverloadedStrings #-}
module ConsoleSpec where

import Interpreter
import Test.Hspec

spec :: Spec
spec = describe "SQL Mini Interpreter" $ do
  it "interprets '.exit' as Exit command" $ do
    interpret ".exit" `shouldBe` Exit
    
  it "interprets 'SELECT 42' as an SqlStatement" $ do
    interpret "SELECT 42" `shouldBe` SqlStatement (Select [ Number 42 ])
  it "interprets 'SELECT 1' as an SqlStatement" $ do
    interpret "SELECT 1" `shouldBe` SqlStatement (Select [ Number 1 ])

  it "interprets 'SELECT Foo, Bar' as a SqlStatement" $ do
    interpret "SELECT Foo, Bar" `shouldBe` SqlStatement (Select [ Col "Foo", Col "Bar"] )
    
  it "interprets unknown string  as Unknown command" $ do
    interpret "foo" `shouldBe` Unknown "(line 1, column 1):\nunexpected \"f\"\nexpecting \"SELECT\""
