{-# LANGUAGE OverloadedStrings #-}
module Sql
  ( SqlStatement(..), Expr(..)
  , prettyPrint, sqlParser
  )
where

import           Data.Monoid
import           Data.Text
import           Text.Parsec
import           Text.Parsec.Char

data Expr = Number Int
  deriving (Eq, Show, Read)

data SqlStatement = Select Expr
  deriving (Eq, Show, Read)

prettyPrint :: SqlStatement -> Text
prettyPrint (Select (Number n)) = "SELECT " <> pack (show n)

type ParserResult a = Either Text  a

sqlParser :: Text -> ParserResult SqlStatement
sqlParser statement =
 case runParser parser () "<STDIN>" statement of
   Left err     -> Left (pack $ show err)
   Right parsed -> Right parsed
  where
    integer = do
      s <- option (1 :: Int) (char '-' >> return (-1))
      dig <- read <$> many1 digit
      return $ s * dig

    parser = do
      string "SELECT"
      space
      n <- integer
      return $ Select (Number n)
