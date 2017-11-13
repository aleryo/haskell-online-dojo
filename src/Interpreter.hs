{-# LANGUAGE OverloadedStrings #-}
module Interpreter
    ( -- * Top-level CLI
      console

      -- * Interpreter
    , Command(..), Sql(..), Expr(..), interpret
    )
where

import Data.Text
import Control.Monad(forever)
import Data.Monoid((<>))
import Text.Parsec

data Expr = Number Int
  deriving (Eq, Show)

data Sql = Select Expr
  deriving (Eq, Show)

data Command = Exit
             | SqlStatement Sql
             | Unknown Text
  deriving (Eq, Show)

interpret :: Text -> Command
interpret ".exit" = Exit
interpret s = parseSQL s

console :: IO ()
console = do
  putStr "> "
  line <- getLine
  let output = interpret $ pack line
  case output of
    Exit        -> putStrLn "bye !"
    Unknown com -> do
      putStrLn ("Unknown command : " <> unpack com)
      console

parseSQL :: Text -> Command
parseSQL text =
  case parse monParser "" text
  of
    Left error   -> Unknown (pack $ show error)
    Right result -> SqlStatement result
  where monParser = do
          string "SELECT"
          spaces
          n <- integer
          return (Select $ Number n)

        integer = fmap read (many1 digit)
