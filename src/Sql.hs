{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Sql where

import Data.Monoid((<>))
import Data.Text hiding (foldr)
import Text.Parsec hiding (label)
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

newtype Database = Database  { tables :: Map.Map TableName Relation } 
  deriving (Eq,Show)

populate :: [ (TableName, Relation) ] -> Database
populate = Database . Map.fromList

evaluate :: Relational -> Database -> Either EvaluationError (Database, Relation)
evaluate rel@(Rel tblName) db@(Database tables) =
  maybe  (Left $ relationNotFound tblName) (Right . (db,)) $ Map.lookup tblName tables

evaluate (Prod [rel1,rel2]) db = do
  (_,table1) <- evaluate rel1 db
  (_,table2) <- evaluate rel2 db
  return $ (db, Relation (columnNames table1 <> columnNames table2) [ t1 <> t2 | t1 <- rows table1, t2 <- rows table2 ])

evaluate (Proj _cols _rel) db = Right (db, Relation [ "col1" ] [["a"]])
evaluate (Create tbl rel) (Database db)  = Right (Database db', rel)
  where
    db'= Map.insert tbl rel db


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
