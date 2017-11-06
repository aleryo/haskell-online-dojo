{-# LANGUAGE OverloadedStrings #-}
module Interpreter
    ( -- * Top-level CLI
      console
      -- * Interpreter
    , Command(..), interpret
    , SqlStatement(..), Expr(..)
    )
where

import           Control.Monad (forever)
import           Data.Monoid   ((<>))
import           Data.Text

data Expr = Number Int
  deriving (Eq, Show, Read)

data SqlStatement = Select Expr
  deriving (Eq, Show, Read)

data Command = Exit
             | Sql SqlStatement
             | Unknown Text
  deriving (Eq, Show, Read)

interpret :: Text -> Command
interpret ".exit"     = Exit
interpret "SELECT 42" = Sql $ Select (Number 42)
interpret unknown     = Unknown unknown

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
