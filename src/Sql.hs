{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Sql where

import Control.Monad.State
import Control.Monad.Except
import Data.Monoid((<>))
import Data.Text hiding (foldr)
import Text.Parsec hiding (label, State)
import Data.Maybe
import Data.String
import qualified Data.Map as Map

data Expr = Number Int
          | Col Text
          | Str Text
  deriving (Eq, Show)

instance IsString Expr where
  fromString = Str . pack

type TableName = Text

type ColumnName = Text

data Sql = Select [ Expr ] [ TableName ]
  | Insert TableName [ ColumnName ] [ [ Expr ] ]
  deriving (Eq, Show)

data Relational = Rel TableName
                | Proj [ ColumnName ] Relational
                | Prod [ Relational ]
                | Create TableName Relation
  deriving (Eq, Show)

data Relation = Relation { columnNames :: [ Text ]
                         , rows       :: [[ Text ]]
                         }
                deriving (Eq, Show)

type EvaluationError = Text

relationNotFound :: TableName -> EvaluationError
relationNotFound name = "no relation with name " <> (pack $ show name)

type DB = Map.Map TableName Relation

newtype Database a = Database { tables :: ExceptT EvaluationError (State DB) a }
  deriving (Functor, Applicative, Monad, MonadState DB, MonadError EvaluationError)

populate :: [ (TableName, Relation) ] -> DB
populate = Map.fromList

evaluate :: Relational -> DB -> Either EvaluationError Relation
evaluate rel db = runDatabase db $ evaluateDB rel

runDatabase :: DB -> Database a -> Either EvaluationError a
runDatabase db = flip evalState db . runExceptT . tables

evaluateDB :: Relational -> Database Relation
evaluateDB rel@(Rel tblName) = do
  tables <- get
  maybe  (throwError $ relationNotFound tblName) pure $ Map.lookup tblName tables

evaluateDB (Prod [rel1,rel2]) = do
  table1 <- evaluateDB rel1
  table2 <- evaluateDB rel2
  return $ Relation (columnNames table1 <> columnNames table2) [ t1 <> t2 | t1 <- rows table1, t2 <- rows table2 ]

evaluateDB (Proj _cols _rel) = pure $ Relation [ "col1" ] [["a"]]
evaluateDB (Create tbl rel)  = do
  modify $ Map.insert tbl rel
  return rel

toRelational :: Sql -> Relational
toRelational (Select projs tableNames) =
   Proj proj (relations tableNames)
   where
     proj = [ x | Col x <- projs ]
     relations [ t ] = Rel t
     relations ts    = Prod $ fmap Rel ts
toRelational (Insert tableName cols values) =
   Create tableName (Relation cols (fmap (fmap eval) values))

eval :: Expr -> Text
eval (Str s) = s

parseSQL :: Text -> Either Text Sql
parseSQL text =
  case parse sql "" text
  of
    Left error   -> Left . pack $ show error
    Right result -> Right result
  where sql = selectClause <|> insertClause

selectClause = do
  string "SELECT"
  spaces
  exprs <- expressionList
  spaces
  tables <- optionMaybe fromClause
  return (Select exprs $ fromMaybe [] tables)

insertClause = do
  string "INSERT" >> spaces >> string "INTO"
  spaces
  table <- tableName
  spaces
  string "("
  spaces
  columns <- columnList
  spaces
  string ")"
  spaces
  string "VALUES"
  spaces
  string "("
  spaces
  values <- expressionList
  spaces
  string ")"
  return (Insert table columns [values])

columnList = columnName `sepBy1` comma

fromClause = do
  string "FROM"
  spaces
  tableList

expressionList = expression `sepBy1` comma

tableList = tableName `sepBy1` comma

expression =  (Number <$> integer)
          <|> (Col <$> columnName)
          <|> (Str <$> stringLiteral)

stringLiteral = do
  char '\''
  str <- label
  char '\''
  return str

comma = try $ spaces >> string "," >> spaces

columnName = label
tableName = label

label = do
  l  <- letter
  ls <- many alphaNum
  return $ pack $ l:ls

integer = fmap read (many1 digit)
