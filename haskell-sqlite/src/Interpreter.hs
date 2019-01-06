{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
import           Sql.DB
import           Sql.DB.VectorDB

data Command = Exit
             | SqlStatement Sql
             | Unknown Text
  deriving (Eq, Show)

interpret :: Text -> Command
interpret ".exit" = Exit
interpret s = case parseSQL s of
                Left err  -> Unknown err
                Right sql -> SqlStatement sql

runCommand :: (Tables db) => Text -> State db (Maybe Text)
runCommand line = do
  db <- get
  let output = interpret line
  case output of
       SqlStatement sql ->
         let (result, db') = execDatabase db (evaluateDB $ toRelational sql)
         in put db' >> return (Just $pack $ show result)
       Unknown err     -> return $ Just $ "unknown command:" <> err
       Exit            -> return $ Nothing

console' :: (Tables tables, Persistable tables io, MonadIO io) => tables -> io ()
console' db = do
  liftIO $ putStr "> "
  line <- pack <$> liftIO getLine
  let (output,db')  = runState (runCommand line) db
  saveDB db'
  case output of
    Nothing  -> liftIO $ IO.putStrLn "bye!"
    Just msg -> liftIO ( IO.putStrLn msg) >> console' db'

console :: IO ()
console = loadDB @BytesDB >>= console'
