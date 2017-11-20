module Sql where

import Data.Text hiding (foldr)
import Text.Parsec hiding (label)
import Data.Maybe

data Expr = Number Int
          | Col Text
  deriving (Eq, Show)

type TableName = Text

type ColumnName = Text

data Sql = Select [ Expr ] [ TableName ]
  deriving (Eq, Show)

data Relational = Rel TableName
                | Proj ColumnName Relational
  deriving (Eq, Show)
                  
toRelational :: Sql -> Relational
toRelational (Select projs [ tableName ]) =
   foldr proj (Rel tableName) projs
   where
     proj (Col t) rel = Proj t rel
     proj _ rel = rel
   
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
