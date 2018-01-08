 {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Sql.Parser (Sql(..), Expr(..), parseSQL) where

import Control.Monad.Identity
import Data.Text hiding (foldr)
import Text.Parsec hiding (label, State)
import Data.Maybe
import Data.String

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


parseSQL :: Text -> Either Text Sql
parseSQL text =
  case parse sql "" text
  of
    Left err     -> Left . pack $ show err
    Right result -> Right result
  where sql = selectClause <|> insertClause

type Parser a = ParsecT Text () Identity a

string_ :: String -> Parser ()
string_ = void . string

char_ :: Char -> Parser ()
char_ = void . char

selectClause :: Parser Sql
selectClause = do
  string_ "SELECT"
  spaces
  exprs <- expressionList
  spaces
  tables <- optionMaybe fromClause
  return (Select exprs $ fromMaybe [] tables)

insertClause :: Parser Sql
insertClause = do
  string_ "INSERT" >> spaces >> string_ "INTO"
  spaces
  table <- tableName
  spaces
  columns <- betweenParens columnList
  spaces >> string_ "VALUES" >> spaces
  values <- betweenParens expressionList
  return (Insert table columns [values])

betweenParens :: Parser a -> Parser a
betweenParens inner = do
  string_ "("
  spaces
  res <- inner
  spaces
  string_ ")"
  return res

columnList :: Parser [ ColumnName ]
columnList = columnName `sepBy1` comma

fromClause :: Parser [ TableName ]
fromClause = do
  string_ "FROM"
  spaces
  tableList

expressionList :: Parser [Expr]
expressionList = expression `sepBy1` comma

tableList :: Parser [Text]
tableList = tableName `sepBy1` comma

expression :: Parser Expr
expression =  (Number <$> integer)
          <|> (Col <$> columnName)
          <|> (Str <$> stringLiteral)

stringLiteral :: Parser Text
stringLiteral = betweenQuotes label

betweenQuotes :: Parser a -> Parser a
betweenQuotes = between (char_ '\'')  (char_ '\'')

comma :: Parser ()
comma = try $ spaces >> string_ "," >> spaces

columnName :: Parser Text
columnName = label

tableName :: Parser Text
tableName = label

label :: Parser Text
label = do
  l  <- letter
  ls <- many alphaNum
  return $ pack $ l:ls

integer :: Parser Int
integer = fmap read (many1 digit)
