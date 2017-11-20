{-# LANGUAGE OverloadedStrings #-}
module ConsoleSpec where

import Interpreter
import Sql
import Test.Hspec

spec :: Spec
spec = describe "SQL Mini Interpreter" $ do
  it "interprets '.exit' as Exit command" $ do
    interpret ".exit" `shouldBe` Exit

  describe "SQL Parser"$ do
    it "interprets 'SELECT 42' as an SqlStatement" $ do
      interpret "SELECT 42" `shouldBe` SqlStatement (Select [ Number 42 ] [])
    it "interprets 'SELECT 1' as an SqlStatement" $ do
      interpret "SELECT 1" `shouldBe` SqlStatement (Select [ Number 1 ] [])

    it "interprets 'SELECT Foo, Bar' as a SqlStatement" $ do
      interpret "SELECT Foo,Bar" `shouldBe` SqlStatement (Select [ Col "Foo", Col "Bar"] [])

    it "interprets 'SELECT Foo, Bar FROM baz' as a SqlStatement" $ do
      interpret "SELECT Foo  , Bar FROM baz"
        `shouldBe` SqlStatement (Select [ Col "Foo", Col "Bar"] ["baz"] )
    it "interprets unknown string  as Unknown command" $ do
      interpret "foo" `shouldBe` Unknown "(line 1, column 1):\nunexpected \"f\"\nexpecting \"SELECT\""

  describe "SQL To Relational" $ do
    it "converts a simple Select statement" $ do
      toRelational (Select [ Col "Foo", Col "Bar"] ["baz"] )
        `shouldBe` Proj "Foo" (Proj "Bar" (Rel "baz"))
        
    it "converts a select statement with miultiple from" $ do
      toRelational (Select [ Col "Foo", Col "Bar"] ["baz", "qix"] )
        `shouldBe` Proj "Foo" (Proj "Bar" (Rel "baz"))
