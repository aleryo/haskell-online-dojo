{-# LANGUAGE OverloadedStrings #-}
module ConsoleSpec where

import           Control.Monad.State
import           Data.Monoid         ((<>))
import           Data.Text
import           Interpreter
import           Sql
import           Sql.DB.MapDB
import           Test.Hspec


-- TODO
-- * durability of DB:
--   * serialize to/from binary: relatively simple, define a binary form then RW
--       -> [x] serialize/deserialize binary to file
--       -> map binary DB to file (e.g. mmap)
--   * store modification journal
--       -> expose modification operations as independent "DSL"
--       -> keep db log/ reload from log (event store) -> transaction
--   * operate DB ops on a "more efficient" representation (e.g. BTree)
--       -> [x] naive way: an array of binary data
--       -> efficient way : Btree structure
-- * Bugs:
--   * [X] handle only 2 tables in product
--   * [ ] handle only 1 column in projection
--   * nsert overwrites previous values
--       -> [X] separate create from insert (new command)
--       -> [X] improve insert to lookup existing table
--   * fromJust when projecting -> unknown column name?
--       -> [X] error handling
-- * Improvements:
--   * Rename DB -> Tables
--   * Better type name for Rows

spec :: Spec
spec = describe "SQL Mini Interpreter" $ do

  let
    populateMapDB rels = populate rels :: MapDB

  it "interprets '.exit' as Exit command" $ do
    interpret ".exit" `shouldBe` Exit

  it "interprets SQL commands" $ do
    let output = do
          _ <- runCommand "CREATE TABLE Foo (Col1)"
          _ <- runCommand "INSERT INTO Foo (Col1) VALUES ('helli')"
          _ <- runCommand "INSERT INTO Foo (Col1) VALUES ('hello')"
          runCommand "SELECT Col1 FROM Foo"

    evalState output (populateMapDB [])
      `shouldBe` Just (pack $ show $ (Right (Relation ["Col1"] [["helli"], ["hello"]]) :: Either Text Relation))

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
      interpret "foo" `shouldBe` Unknown "(line 1, column 1):\nunexpected \"f\"\nexpecting \"SELECT\", \"INSERT\" or \"CREATE\""

    it "interprets INSERT INTO Foo (Col1) VALUES ('hello') as a SqlStatement" $ do
      interpret "INSERT INTO Foo (Col1) VALUES ('hello')"
         `shouldBe` SqlStatement (Insert "Foo" [ "Col1" ] [ [ "hello" ] ])

    it "interprets CREATE TABLE  Foo (Col1) as a SqlStatement" $ do
      interpret "CREATE TABLE Foo (Col1)"
         `shouldBe` SqlStatement (CreateTable "Foo" [ "Col1" ])

  describe "SQL To Relational" $ do
    it "converts a simple Select statement" $ do
      toRelational (Select [ Col "Foo", Col "Bar"] ["baz"] )
        `shouldBe` Proj  [ "Foo", "Bar"] (Rel "baz")

    it "converts a select statement with multiple from" $ do
      toRelational (Select [ Col "Foo", Col "Bar"] ["baz", "qix"] )
        `shouldBe` Proj [ "Foo", "Bar"] (Prod [ Rel "baz", Rel "qix"])

    it "converts an insert statement" $ do
      toRelational (Insert "Foo" [ "Col1" ] [ [ "hello"] ])
      `shouldBe` Append "Foo" (Relation [ "Col1" ] [["hello"]])

    it "converts an create table statement" $ do
      toRelational (CreateTable "Foo" [ "Col1" ] )
      `shouldBe` Create "Foo" [ "Col1" ]

  describe "Expression evaluation" $ do

    let relationabc = Relation [ "col1", "col2", "col3"] [["a", "b", "c"]]
        relationdef = Relation [ "col4" ] [["def"]]
        relationghc = Relation [ "col5" ] [["ghc (pun intended)"]]

    it "evaluates a relation" $ do
      let  db = populateMapDB [ ( "Foo", relationabc) ]
      evaluate (Rel "Foo") db
        `shouldBe` Right relationabc

    it "evaluates another relation" $ do
      let db = populateMapDB [ ( "Bar", relationdef) ]
      evaluate (Rel "Bar") db
        `shouldBe` Right relationdef

    it "evaluates a  relation in a database with several tables" $ do
      let db = populateMapDB [ ( "Foo", relationabc)
                        , ( "Bar", relationdef)
                        ]
      evaluate (Rel "Bar") db
        `shouldBe` Right relationdef

    it "returns error when evaluating relation given relation is not in DB" $ do
      let db = populateMapDB []
      evaluate (Rel "Bar") db
        `shouldBe` Left "no table with name \"Bar\""

    it "evaluates cartesian product of 3 relations" $ do
      let db = populateMapDB [ ("Foo", relationabc)
                             , ("Bar", relationdef)
                             , ("Baz", relationghc) ]

      evaluate (Prod [ Rel "Foo", Rel "Bar", Rel "Baz"]) db
        `shouldBe` Right (Relation [ "col1", "col2", "col3", "col4", "col5" ]
                           [row1 <> row2 <> row3 | row1 <- [["a", "b", "c"]]
                                                 , row2 <- [["def"]]
                                                 , row3 <- [["ghc (pun intended)"]]])

    it "returns error when evaluating cartesian product given one relation does not exist" $ do
      let db = populateMapDB [ ("Foo", relationabc) ]
      evaluate (Prod [ Rel "Foo", Rel "Bar"]) db
        `shouldBe` Left "no table with name \"Bar\""

    it "filter columns when evaluating select clause" $ do
      let  db = populateMapDB [ ( "Foo", relationabc) ]
      evaluate (Proj [ "col1"] (Rel "Foo")) db
        `shouldBe` Right (Relation [ "col1" ] [["a"]])

    it "filter columns when evaluating select clause" $ do
      let  db = populateMapDB [ ( "Foo", relationabc) ]
      evaluate (Proj [ "col1", "col2"] (Rel "Foo")) db
        `shouldBe` Right (Relation [ "col1", "col2" ] [["a", "b"]])

    it "returns error when filtering columns on SELECT given column does not exist" $ do
      let  db = populateMapDB [ ( "Foo", relationabc) ]
      evaluate (Proj [ "col4"] (Rel "Foo")) db
        `shouldBe` Left "no column with name \"col4\""

    it "creates a table with input data" $ do
      let db = populateMapDB []

      evaluate (Create "Foo" [ "Col1" ]) db
        `shouldBe` Right (Relation [ "Col1"] [])

    it "insert data into an existing table" $ do
      let db = populateMapDB []
          sql = do
            _ <- evaluateDB (Create "Foo" [ "Col1" ])
            _ <- evaluateDB (Append "Foo" (Relation [ "Col1" ] [ [ "helli" ]]))
            _ <- evaluateDB (Append "Foo" (Relation [ "Col1" ] [ [ "hello" ]]))
            evaluateDB (Rel "Foo")

      runDatabase db sql
        `shouldBe` Right (Relation [ "Col1"] [ [ "helli"] , ["hello"] ])

    it "fails to insert data when columns don't match" $ do
      let db = populateMapDB []
          sql = do
            _ <- evaluateDB (Create "Foo" [ "Col1" ])
            _ <- evaluateDB (Append "Foo" (Relation [ "Col1" ] [ [ "helli" ]]))
            _ <- evaluateDB (Append "Foo" (Relation [ "Col2" ] [ [ "hello" ]]))
            evaluateDB (Rel "Foo")

      runDatabase db sql
        `shouldBe` Left "Incompatible relation schemas"

    it "evaluates a select over a create and insert" $ do
      let db = populateMapDB []
          sql = do
            _ <- evaluateDB (Create "Foo" [ "Col1" ])
            _ <- evaluateDB (Create "Bar" [ "Col2" ])
            _ <- evaluateDB (Append "Foo" (Relation [ "Col1" ] [ [ "hello" ]]))
            _ <- evaluateDB (Append "Bar" (Relation [ "Col2" ] [ [ "helli" ]]))
            evaluateDB (Prod [ Rel "Foo", Rel "Bar"])

      runDatabase db sql
        `shouldBe` Right (Relation [ "Col1" ,"Col2"] [ [ "hello", "helli"] ])
