{-# LANGUAGE OverloadedStrings #-}
module Interpreter
    ( -- * Top-level CLI
      console

      -- * Interpreter
    , Command(..), Sql(..), Expr(..), interpret
    )
where

import Data.Text
import Data.Monoid((<>))
import Sql

data Command = Exit
             | SqlStatement Sql
             | Unknown Text
  deriving (Eq, Show)

interpret :: Text -> Command
interpret ".exit" = Exit
interpret s = case parseSQL s of
                Left err -> Unknown err
                Right sql -> SqlStatement sql

console :: IO ()
console = do
  putStr "> "
  line <- getLine
  let output = interpret $ pack line
      db = populate []
  case output of
    Exit             -> putStrLn "bye !"
    SqlStatement sql -> do
      print $ runDatabase db (evaluateDB $ toRelational sql)
      console
    Unknown com -> do
      putStrLn ("Unknown command : " <> unpack com)
      console
