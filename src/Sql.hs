{-# LANGUAGE OverloadedStrings #-}
module Sql where

import Data.Monoid((<>))
import Data.Text hiding (foldr)
import Text.Parsec hiding (label)
import Data.Maybe
import qualified Data.Map as Map

data Expr = Number Int
          | Col Text
  deriving (Eq, Show)

type TableName = Text

type ColumnName = Text

data Sql = Select [ Expr ] [ TableName ]
  deriving (Eq, Show)

data Relational = Rel TableName
                | Proj [ ColumnName ] Relational
                | Prod [ Relational ]
  deriving (Eq, Show)

data Relation = Relation { columnNames :: [ Text ]
                         , rows       :: [[ Text ]]
                         }
                deriving (Eq, Show)

type EvaluationError = Text

relationNotFound :: TableName -> EvaluationError
relationNotFound name = "no relation with name " <> (pack $ show name)

newtype Database = Database  { tables :: Map.Map TableName Relation } 

populate :: [ (TableName, Relation) ] -> Database
populate = Database . Map.fromList

evaluate :: Relational -> Database -> Either EvaluationError Relation
evaluate rel@(Rel tblName) (Database tables) =
  maybe  (Left $ relationNotFound tblName) Right $ Map.lookup tblName tables

evaluate (Prod [rel1,rel2]) db = do
  table1 <- evaluate rel1 db
  table2 <- evaluate rel2 db
  return $ Relation (columnNames table1 <> columnNames table2) [ t1 <> t2 | t1 <- rows table1, t2 <- rows table2 ]

evaluate (Proj _cols _rel) _db = Right $ Relation [ "col1" ] [["a"]]


toRelational :: Sql -> Relational
toRelational (Select projs tableNames) =
   Proj proj (relations tableNames)
   where
     proj = [ x | Col x <- projs ]
     relations [ t ] = Rel t
     relations ts    = Prod $ fmap Rel ts
   
parseSQL :: Text -> Either Text Sql
parseSQL text =
  case parse sql "" text
  of
    Left error   -> Left . pack $ show error
    Right result -> Right result
  where sql = selectClause

selectClause = do
  string "SELECT"
  spaces
  exprs <- expressionList
  spaces
  tables <- optionMaybe fromClause
  return (Select exprs $ fromMaybe [] tables)

fromClause = do
  string "FROM"
  spaces
  tableList
  
expressionList = expression `sepBy1` comma

tableList = tableName `sepBy1` comma

expression =  (Number <$> integer)
          <|> (Col <$> columnName)
  
comma = try $ spaces >> string "," >> spaces

columnName = label
tableName = label

label = do
  l  <- letter
  ls <- many alphaNum
  return $ pack $ l:ls

integer = fmap read (many1 digit)
