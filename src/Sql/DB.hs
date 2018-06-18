{-# LANGUAGE MultiParamTypeClasses #-}
module Sql.DB where

import           Data.Text (Text)

type TableName = Text

type ColumnName = Text

type Row = [ Text ]

data Relation = Relation { columnNames :: [ColumnName]
                         , rows        :: [Row]
                         }
                deriving (Eq, Show)


class DB db where
  initDB :: db
  lookup :: TableName             -> db -> Maybe Relation
  insert :: TableName -> Relation -> db -> db

populate :: (DB db) => [ (TableName, Relation) ] -> db
populate = foldr (\ (tbl,rel) db -> insert tbl rel db) initDB


-- * Persistence
class (DB db, Monad m) => Persistable db m where
  saveDB :: db -> m ()
  loadDB :: m db
