module Sql where

import Data.Text
import Text.Parsec

data Expr = Number Int
  deriving (Eq, Show)

data Sql = Select Expr
  deriving (Eq, Show)

parseSQL :: Text -> Either Text Sql
parseSQL text =
  case parse monParser "" text
  of
    Left error   -> Left . pack $ show error
    Right result -> Right result
  where monParser = do
          string "SELECT"
          spaces
          n <- integer
          return (Select $ Number n)

        integer = fmap read (many1 digit)
