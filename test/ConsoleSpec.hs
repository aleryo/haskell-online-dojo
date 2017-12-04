{-# LANGUAGE OverloadedStrings #-}
module ConsoleSpec where

import Data.Monoid((<>))
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
        `shouldBe` Proj  [ "Foo", "Bar"] (Rel "baz")

    it "converts a select statement with multiple from" $ do
      toRelational (Select [ Col "Foo", Col "Bar"] ["baz", "qix"] )
        `shouldBe` Proj [ "Foo", "Bar"] (Prod [ Rel "baz", Rel "qix"])

  describe "Expression evaluation" $ do
    
    it "evaluates a relation" $ do
      let  db = populate [ ( "Foo", Relation [ "col1", "col2", "col3"] [["a"], ["b"], ["c"]]) ]
      evaluate (Rel "Foo") db 
        `shouldBe` Right (Relation [ "col1", "col2", "col3"] [["a"], ["b"], ["c"]])
        
    it "evaluates another relation" $ do
      let db = populate [ ( "Bar", Relation [ "col4", "col5", "col6"] [["d"], ["e"], ["f"]]) ]
      evaluate (Rel "Bar") db 
        `shouldBe` Right (Relation [ "col4", "col5", "col6"][["d"], ["e"], ["f"]])

    it "evaluates a  relation in a database with several tables" $ do
      let db = populate [ ("Foo", Relation [ "col1", "col2", "col3"] [["a"], ["b"], ["c"]])
                        , ( "Bar", Relation [ "col4", "col5", "col6"] [["d"], ["e"], ["f"]])
                        ]
      evaluate (Rel "Bar") db 
        `shouldBe` Right (Relation [ "col4", "col5", "col6"] [["d"], ["e"], ["f"]])

    it "returns error when evaluating relation given relation is not in DB" $ do
      let db = populate []
      evaluate (Rel "Bar") db 
        `shouldBe` Left "no relation with name \"Bar\""

    it "evaluates cartesian product of 2 relations" $ do
      let db = populate [ ("Foo", Relation [ "col1", "col2", "col3"] [["a"], ["b"], ["c"]])
                        , ( "Bar", Relation [ "col4", "col5", "col6"] [["d"], ["e"], ["f"]])
                        ]
      evaluate (Prod [ Rel "Foo", Rel "Bar"]) db 
        `shouldBe` Right (Relation [ "col1", "col2", "col3", "col4", "col5", "col6"]
                           [t1 <> t2 | t1 <- [["a"], ["b"], ["c"]]
                                     , t2 <- [["d"], ["e"], ["f"]]] )

    it "returns error when evaluating cartesian product given one relation does not exist" $ do
      let db = populate [ ("Foo", Relation [ "col1", "col2", "col3"] [["a"], ["b"], ["c"]]) ]
      evaluate (Prod [ Rel "Foo", Rel "Bar"]) db 
        `shouldBe` Left "no relation with name \"Bar\""
