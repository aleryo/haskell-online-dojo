{-# LANGUAGE OverloadedStrings #-}

module Interpreter
    ( -- * Top-level CLI
      console
    , runCommand
      -- * Interpreter
    , Command(..), Sql(..), Expr(..), interpret
    )
where

import           Control.Monad.State
import           Data.Monoid         ((<>))
import           Data.Text
import qualified Data.Text.IO        as IO
import           Sql
import           Sql.DB.MapDB

data Command = Exit
             | SqlStatement Sql
             | Unknown Text
  deriving (Eq, Show)

interpret :: Text -> Command
interpret ".exit" = Exit
interpret s = case parseSQL s of
                Left err  -> Unknown err
                Right sql -> SqlStatement sql

runCommand :: (DB db) => Text -> State db (Maybe Text)
runCommand line = do
  db <- get
  let output = interpret line
  case output of
       SqlStatement sql ->
         let (result, db') = execDatabase db (evaluateDB $ toRelational sql)
         in put db' >> return (Just $pack $ show result)
       Unknown err     -> return $ Just $ "unknown command:" <> err
       Exit            -> return $ Nothing

console' :: (DB db) => db -> IO ()
console' db = do
  putStr "> "
  line <- pack <$> getLine
  let (output,db')  = runState (runCommand line) db
  case output of
    Nothing  -> IO.putStrLn "bye!"
    Just msg -> IO.putStrLn msg >> console' db'

loadDB :: IO MapDB
loadDB = pure initDB

console :: IO ()
console = loadDB >>= console'
