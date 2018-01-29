{-# LANGUAGE OverloadedStrings #-}
module Interpreter
    ( -- * Top-level CLI
      console
    , runCommand
      -- * Interpreter
    , Command(..), Sql(..), Expr(..), interpret
    )
where

import Data.Text
import qualified Data.Text.IO as IO
import Data.Monoid((<>))
import Sql

data Command = Exit
             | SqlStatement Sql
             | Unknown Text
  deriving (Eq, Show)

interpret :: Text -> Command
interpret ".exit" = Exit
interpret s = case parseSQL s of
                Left err  -> Unknown err
                Right sql -> SqlStatement sql

runCommand :: Text -> DB -> Maybe (Text, DB)
runCommand line db =
  let output = interpret line
  in case output of
       SqlStatement sql ->
         let (result, db') = execDatabase db (evaluateDB $ toRelational sql)
         in Just (pack $ show result, db')
       Unknown err     -> Just ("unknown command:" <> err, db)
       Exit           -> Nothing


console :: IO ()
console = console' (populate [])
  where
    console' db = do
      putStr "> "
      line <- pack <$> getLine
      let output = runCommand line db
      case output of
        Nothing         -> IO.putStrLn "bye!"
        Just (msg, db') -> IO.putStrLn msg >> console' db'
