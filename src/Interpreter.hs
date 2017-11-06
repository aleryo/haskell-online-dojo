{-# LANGUAGE OverloadedStrings #-}
module Interpreter
    ( -- * Top-level CLI
      console
      -- * Interpreter
    , Command(..), interpret, isUnknown
    , module Sql
    )
where

import           Control.Monad (forever)
import           Data.Monoid   ((<>))
import           Data.Text
import           Sql

data Command = Exit
             | Sql SqlStatement
             | Unknown Text
  deriving (Eq, Show, Read)

isUnknown :: Command -> Bool
isUnknown (Unknown _) = True
isUnknown _           = False

interpret :: Text -> Command
interpret ".exit" = Exit
interpret input   =
  case sqlParser input of
    Right sqlStatement -> Sql sqlStatement
    Left  err          -> Unknown err

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
