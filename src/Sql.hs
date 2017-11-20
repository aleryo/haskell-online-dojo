module Sql where

import Data.Text
import Text.Parsec

data Expr = Number Int
          | Col Text
  deriving (Eq, Show)

data Sql = Select [ Expr ]
  deriving (Eq, Show)

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

          return (Select exprs)

        expressionList = expression `sepBy1` comma

        expression =  (Number <$> integer)
                  <|> (Col <$> columnName)
          
        comma = spaces >> string ","  >> spaces

        columnName = do
          l  <- letter
          ls <- many alphaNum
          return $ pack $ l:ls
          
        integer = fmap read (many1 digit)
