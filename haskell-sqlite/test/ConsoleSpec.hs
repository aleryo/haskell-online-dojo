{-# LANGUAGE OverloadedStrings #-}

module ConsoleSpec where

import           Control.Monad.State
import           Data.Monoid         ((<>))
import           Data.Text
import           Interpreter
import           Sql
import           Sql.DB.MapDB
import           Test.Hspec
import           Test.QuickCheck

-- TODO
-- * durability of DB:
--   * serialize to/from binary: relatively simple, define a binary form then RW
--       -> [x] serialize/deserialize binary to file
--       -> map binary DB to file (e.g. mmap)
--   * introduce a FS-like structure in storage
--       -> direct access to each table
--       -> add indices
--   * store modification journal
--       -> expose modification operations as independent "DSL"
--       -> keep db log/ reload from log (event store) -> transaction
--   * operate DB ops on a "more efficient" representation (e.g. BTree)
--       -> [x] naive way: an array of binary data
--       -> efficient way
-- * extend SQL:
--    * [X] support WHERE
-- * Bugs:
--   * [X] handle only 2 tables in product
--   * [X] handle only 1 column in projection
--   * Insert overwrites previous values
--       -> [X] separate create from insert (new command)
--       -> [X] improve insert to lookup existing table
--   * fromJust when projecting -> unknown column name?
--       -> [X] error handling
-- * Improvements:
--   * [X] Rename DB -> Tables
--   * Better type name for Rows
spec :: Spec
spec =
  describe "SQL Mini Interpreter" $ do
    let populateMapDB rels = populate rels :: MapDB
    it "interprets '.exit' as Exit command" $ interpret ".exit" `shouldBe` Exit
    it "interprets SQL commands" $ do
      let output = do
            _ <- runCommand "CREATE TABLE Foo (Col1)"
            _ <- runCommand "INSERT INTO Foo (Col1) VALUES ('helli')"
            _ <- runCommand "INSERT INTO Foo (Col1) VALUES ('hello')"
            runCommand "SELECT Col1 FROM Foo"
      evalState output (populateMapDB []) `shouldBe`
        Just
          (pack $
           show $
           (Right (Relation ["Col1"] [["helli"], ["hello"]]) :: Either Text Relation))
    describe "SQL Parser" $ do
      it "interprets 'SELECT 42' as an SqlStatement" $
        interpret "SELECT 42" `shouldBe`
        SqlStatement (Select [Number 42] [] Nothing)
      it "interprets 'SELECT 1' as an SqlStatement" $
        interpret "SELECT 1" `shouldBe`
        SqlStatement (Select [Number 1] [] Nothing)
      it "interprets 'SELECT Foo, Bar' as a SqlStatement" $
        interpret "SELECT Foo,Bar" `shouldBe`
        SqlStatement (Select [Col "Foo", Col "Bar"] [] Nothing)
      it "interprets 'SELECT Foo, Bar FROM baz' as a SqlStatement" $
        interpret "SELECT Foo  , Bar FROM baz" `shouldBe`
        SqlStatement (Select [Col "Foo", Col "Bar"] ["baz"] Nothing)
      it "interprets unknown string  as Unknown command" $
        interpret "foo" `shouldBe`
        Unknown
          "(line 1, column 1):\nunexpected \"f\"\nexpecting \"SELECT\", \"INSERT\" or \"CREATE\""
      it "interprets INSERT INTO Foo (Col1) VALUES ('hello') as a SqlStatement" $
        interpret "INSERT INTO Foo (Col1) VALUES ('hello')" `shouldBe`
        SqlStatement (Insert "Foo" ["Col1"] [["hello"]])
      it "interprets CREATE TABLE  Foo (Col1) as a SqlStatement" $
        interpret "CREATE TABLE Foo (Col1)" `shouldBe`
        SqlStatement (CreateTable "Foo" ["Col1"])
      it "interpret WHERE clauses a SqlStatement" $
        interpret "SELECT Foo FROM Bar WHERE Foo = 12" `shouldBe`
        SqlStatement
          (Select [Col "Foo"] ["Bar"] (Just (Equal (Col "Foo") (Number 12))))
    describe "SQL To Relational" $ do
      it "converts a simple Select statement" $
        toRelational (Select [Col "Foo", Col "Bar"] ["baz"] Nothing) `shouldBe`
        Proj ["Foo", "Bar"] (Rel "baz")
      it "converts a simple Select statement with where clause" $
        toRelational
          (Select
             [Col "Foo", Col "Bar"]
             ["baz"]
             (Just (Equal (Col "Foo") (Number 12)))) `shouldBe`
        Proj ["Foo", "Bar"] (Sel (Equal (Col "Foo") (Number 12)) (Rel "baz"))
      it "converts a select statement with multiple from" $
        toRelational (Select [Col "Foo", Col "Bar"] ["baz", "qix"] Nothing) `shouldBe`
        Proj ["Foo", "Bar"] (Prod [Rel "baz", Rel "qix"])
      it "converts an insert statement" $
        toRelational (Insert "Foo" ["Col1"] [["hello"]]) `shouldBe`
        Append "Foo" (Relation ["Col1"] [["hello"]])
      it "converts an create table statement" $
        toRelational (CreateTable "Foo" ["Col1"]) `shouldBe`
        Create "Foo" ["Col1"]
    describe "Select Expression Evaluation" $ do
      it "evaluates equality predicate over string when column exists" $ do
        evalExpr (Equal (Str "a") (Col "col")) ["col"] ["a"] `shouldBe`
          Right True
        evalExpr (Equal (Col "col") (Str "a")) ["col"] ["a"] `shouldBe`
          Right True
        evalExpr (Equal (Str "a") (Col "col")) ["col1", "col"] ["b", "a"] `shouldBe`
          Right True
      it "raises an error when a column in predicate does not exist" $
        evalExpr (Equal (Str "a") (Col "col2")) ["col1", "col"] ["b", "a"] `shouldBe`
        Left "no column with name \"col2\""
      it "raises an error when expression is malformed" $
        property malformedExpressionRaisesError
    describe "Relational expression evaluation" $ do
      let relationabc = Relation ["col1", "col2", "col3"] [["a", "b", "c"]]
          relationdef = Relation ["col4"] [["def"]]
          relationghc = Relation ["col5"] [["ghc (pun intended)"]]
          relationabcs =
            Relation ["col1", "col2", "col3"] [["a", "b", "c"], ["d", "e", "f"]]
      it "evaluates a relation" $ do
        let db = populateMapDB [("Foo", relationabc)]
        evaluate (Rel "Foo") db `shouldBe` Right relationabc
      it "evaluates another relation" $ do
        let db = populateMapDB [("Bar", relationdef)]
        evaluate (Rel "Bar") db `shouldBe` Right relationdef
      it "evaluates a  relation in a database with several tables" $ do
        let db = populateMapDB [("Foo", relationabc), ("Bar", relationdef)]
        evaluate (Rel "Bar") db `shouldBe` Right relationdef
      it "returns error when evaluating relation given relation is not in DB" $ do
        let db = populateMapDB []
        evaluate (Rel "Bar") db `shouldBe` Left "no table with name \"Bar\""
      it "evaluates cartesian product of 3 relations" $ do
        let db =
              populateMapDB
                [ ("Foo", relationabc)
                , ("Bar", relationdef)
                , ("Baz", relationghc)
                ]
        evaluate (Prod [Rel "Foo", Rel "Bar", Rel "Baz"]) db `shouldBe`
          Right
            (Relation
               ["col1", "col2", "col3", "col4", "col5"]
               [ row1 <> row2 <> row3
               | row1 <- [["a", "b", "c"]]
               , row2 <- [["def"]]
               , row3 <- [["ghc (pun intended)"]]
               ])
      it
        "returns error when evaluating cartesian product given one relation does not exist" $ do
        let db = populateMapDB [("Foo", relationabc)]
        evaluate (Prod [Rel "Foo", Rel "Bar"]) db `shouldBe`
          Left "no table with name \"Bar\""
      it "filter columns when evaluating select clause" $ do
        let db = populateMapDB [("Foo", relationabc)]
        evaluate (Proj ["col1"] (Rel "Foo")) db `shouldBe`
          Right (Relation ["col1"] [["a"]])
      it "filter columns when evaluating select clause" $ do
        let db = populateMapDB [("Foo", relationabc)]
        evaluate (Proj ["col1", "col2"] (Rel "Foo")) db `shouldBe`
          Right (Relation ["col1", "col2"] [["a", "b"]])
      it "select rows columns when evaluating select/where clause" $ do
        let db = populateMapDB [("Foo", relationabcs)]
        evaluate (Sel (Equal (Col "col1") (Str "d")) (Rel "Foo")) db `shouldBe`
          Right (Relation ["col1", "col2", "col3"] [["d", "e", "f"]])
        evaluate (Sel (Equal (Col "col1") (Str "a")) (Rel "Foo")) db `shouldBe`
          Right (Relation ["col1", "col2", "col3"] [["a", "b", "c"]])
      it
        "returns error when filtering columns on SELECT given column does not exist" $ do
        let db = populateMapDB [("Foo", relationabc)]
        evaluate (Proj ["col4"] (Rel "Foo")) db `shouldBe`
          Left "no column with name \"col4\""
      it "creates a table with input data" $ do
        let db = populateMapDB []
        evaluate (Create "Foo" ["Col1"]) db `shouldBe`
          Right (Relation ["Col1"] [])
      it "insert data into an existing table" $ do
        let db = populateMapDB []
            sql = do
              _ <- evaluateDB (Create "Foo" ["Col1"])
              _ <- evaluateDB (Append "Foo" (Relation ["Col1"] [["helli"]]))
              _ <- evaluateDB (Append "Foo" (Relation ["Col1"] [["hello"]]))
              evaluateDB (Rel "Foo")
        runDatabase db sql `shouldBe`
          Right (Relation ["Col1"] [["helli"], ["hello"]])
      it "fails to insert data when columns don't match" $ do
        let db = populateMapDB []
            sql = do
              _ <- evaluateDB (Create "Foo" ["Col1"])
              _ <- evaluateDB (Append "Foo" (Relation ["Col1"] [["helli"]]))
              _ <- evaluateDB (Append "Foo" (Relation ["Col2"] [["hello"]]))
              evaluateDB (Rel "Foo")
        runDatabase db sql `shouldBe` Left "Incompatible relation schemas"
      it "evaluates a select over a create and insert" $ do
        let db = populateMapDB []
            sql = do
              _ <- evaluateDB (Create "Foo" ["Col1"])
              _ <- evaluateDB (Create "Bar" ["Col2"])
              _ <- evaluateDB (Append "Foo" (Relation ["Col1"] [["hello"]]))
              _ <- evaluateDB (Append "Bar" (Relation ["Col2"] [["helli"]]))
              evaluateDB (Prod [Rel "Foo", Rel "Bar"])
        runDatabase db sql `shouldBe`
          Right (Relation ["Col1", "Col2"] [["hello", "helli"]])

evalExpr e c r = runDatabase (emptyTables :: MapDB) $ eval e c r

malformedExpressionRaisesError :: NonBooleanExpression -> Bool
malformedExpressionRaisesError (NonBooleanExpression expr) =
  evalExpr expr [] [] == Left "Expression is not a boolean expression"

newtype NonBooleanExpression =
  NonBooleanExpression Expr
  deriving (Eq, Show)

instance Arbitrary NonBooleanExpression where
  arbitrary = undefined
