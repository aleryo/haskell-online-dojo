{-# LANGUAGE OverloadedStrings #-}
module Sql where

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

newtype Database = Database  { tables :: Map.Map TableName [ Text ] } 

populate :: [ (TableName, [ Text ]) ] -> Database
populate = Database . Map.fromList

evaluate :: Relational -> Database -> [Text]
evaluate rel@(Rel tblName) (Database tables) =
  fromJust $ Map.lookup tblName tables
evaluate _  _ = undefined


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
