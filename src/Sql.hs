{-# LANGUAGE OverloadedStrings #-}
module Sql
  ( SqlStatement(..), Expr(..)
  , sqlParser
  )
where

import           Data.Monoid
import           Data.Text

data Expr = Number Int
  deriving (Eq, Show, Read)

data SqlStatement = Select Expr
  deriving (Eq, Show, Read)


sqlParser :: Text -> Either Text SqlStatement
sqlParser "SELECT 42" = Right $ Select (Number 42)
sqlParser somethingelse = Left $ "cannot parse '" <> somethingelse <> "' as a SQL statement"
