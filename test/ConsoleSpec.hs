{-# LANGUAGE OverloadedStrings #-}
module ConsoleSpec where

import Data.Monoid((<>))
import Interpreter
import Sql
import Test.Hspec
import Control.Monad.State
import Data.Text


-- TODO
-- * durability of DB:
--   * serialize to/from binary: relatively simple, define a binary form then RW
--       -> (2.5) serialize/deserialize binary to file
--       -> (3.5) map binary DB to file (e.g. mmap)
--   * store modification journal
--       -> expose modification operations as independent "DSL"
--       -> (5) keep db log/ reload from log (event store) -> transaction
--   * operate DB ops on a "more efficient" representation (e.g. BTree)
--       -> (2) naive way: an array of records
--       -> (3) efficient way : Btree structure
-- * Bugs:
--   * Insert overwrites previous values
--       -> separate create from insert (new command)
--       -> (1) improve insert to lookup existing table
--   * fromJust when projecting -> unknown column name?
--       -> error handling

spec :: Spec
spec = describe "SQL Mini Interpreter" $ do

  it "interprets '.exit' as Exit command" $ do
    interpret ".exit" `shouldBe` Exit

  it "interprets '.load file.db' as Database load command" $ do
    pending

  it "interprets SQL commands" $ do
    let output = do
          _ <- runCommand "INSERT INTO Foo (Col1) VALUES ('helli')"
          runCommand "SELECT Col1 FROM Foo"

    evalState output (populate []) `shouldBe` Just (pack $ show $ (Right (Relation ["Col1"] [["helli"]]) :: Either Text Relation))

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
      interpret "foo" `shouldBe` Unknown "(line 1, column 1):\nunexpected \"f\"\nexpecting \"SELECT\" or \"INSERT\""

    it "interprets INSERT INTO Foo (Col1) VALUES ('hello') as a SqlStatement" $ do
      interpret "INSERT INTO Foo (Col1) VALUES ('hello')"
         `shouldBe` SqlStatement (Insert "Foo" [ "Col1" ] [ [ "hello" ] ])

  describe "SQL To Relational" $ do
    it "converts a simple Select statement" $ do
      toRelational (Select [ Col "Foo", Col "Bar"] ["baz"] )
        `shouldBe` Proj  [ "Foo", "Bar"] (Rel "baz")

    it "converts a select statement with multiple from" $ do
      toRelational (Select [ Col "Foo", Col "Bar"] ["baz", "qix"] )
        `shouldBe` Proj [ "Foo", "Bar"] (Prod [ Rel "baz", Rel "qix"])

    it "converts an insert statement" $ do
      toRelational (Insert "Foo" [ "Col1" ] [ [ "hello"] ])
      `shouldBe` Create "Foo" (Relation [ "Col1" ] [["hello"]])

  describe "Expression evaluation" $ do

    let relationabc = Relation [ "col1", "col2", "col3"] [["a", "b", "c"]]
        relationdef = Relation [ "col4"] [["d"], ["e"], ["f"]]

    it "evaluates a relation" $ do
      let  db = populate [ ( "Foo", relationabc) ]
      evaluate (Rel "Foo") db
        `shouldBe` Right relationabc

    it "evaluates another relation" $ do
      let db = populate [ ( "Bar", relationdef) ]
      evaluate (Rel "Bar") db
        `shouldBe` Right relationdef

    it "evaluates a  relation in a database with several tables" $ do
      let db = populate [ ( "Foo", relationabc)
                        , ( "Bar", relationdef)
                        ]
      evaluate (Rel "Bar") db
        `shouldBe` Right relationdef

    it "returns error when evaluating relation given relation is not in DB" $ do
      let db = populate []
      evaluate (Rel "Bar") db
        `shouldBe` Left "no relation with name \"Bar\""

    it "evaluates cartesian product of 2 relations" $ do
      let db = populate [ ("Foo", relationabc)
                        , ( "Bar", relationdef)
                        ]
      evaluate (Prod [ Rel "Foo", Rel "Bar"]) db
        `shouldBe` Right (Relation [ "col1", "col2", "col3", "col4" ]
                           [t1 <> t2 | t1 <- [["a", "b", "c"]]
                                     , t2 <- [["d"], ["e"], ["f"]]] )

    it "returns error when evaluating cartesian product given one relation does not exist" $ do
      let db = populate [ ("Foo", relationabc) ]
      evaluate (Prod [ Rel "Foo", Rel "Bar"]) db
        `shouldBe` Left "no relation with name \"Bar\""

    it "filter columns when evaluating select clause" $ do
      let  db = populate [ ( "Foo", relationabc) ]
      evaluate (Proj [ "col1"] (Rel "Foo")) db
        `shouldBe` Right (Relation [ "col1" ] [["a"]])

    it "filter columns when evaluating select clause" $ do
      let  db = populate [ ( "Foo", relationabc) ]
      evaluate (Proj [ "col2"] (Rel "Foo")) db
        `shouldBe` Right (Relation [ "col2" ] [["b"]])

    it "creates a table with input data" $ do
      let db = populate []

      evaluate (Create "Foo" (Relation [ "Col1" ] [ [ "hello" ]])) db
        `shouldBe` Right (Relation [ "Col1"] [ [ "hello"] ])

    it "insert data into an existing table" $ do
      let db = populate []
          sql = do
            _ <- evaluateDB (Create' "Foo" [ "Col1" ])
            _ <- evaluateDB (Add "Foo" (Relation [ "Col1" ] [ [ "helli" ]]))
            _ <- evaluateDB (Add "Foo" (Relation [ "Col1" ] [ [ "hello" ]]))
            evaluateDB (Rel "Foo")

      runDatabase db sql
        `shouldBe` Right (Relation [ "Col1"] [ [ "helli"] , ["hello"] ])

    it "fails to insert data when columns don't match" $ do
      let db = populate []
          sql = do
            _ <- evaluateDB (Create' "Foo" [ "Col1" ])
            _ <- evaluateDB (Add "Foo" (Relation [ "Col1" ] [ [ "helli" ]]))
            _ <- evaluateDB (Add "Foo" (Relation [ "Col2" ] [ [ "hello" ]]))
            evaluateDB (Rel "Foo")

      runDatabase db sql
        `shouldBe` Left "Incompatible relation schemas"

    it "evaluates a select over a create" $ do
      let db = populate []
          sql = do
            _ <- evaluateDB (Create "Foo" (Relation [ "Col1" ] [ [ "hello" ]]))
            _ <- evaluateDB (Create "Bar" (Relation [ "Col2" ] [ [ "helli" ]]))
            evaluateDB (Prod [ Rel "Foo", Rel "Bar"])

      runDatabase db sql
        `shouldBe` Right (Relation [ "Col1" ,"Col2"] [ [ "hello", "helli"] ])
