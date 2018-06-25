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


type Table =  (TableName, Relation)

class Tables tables where
  emptyTables :: tables
  lookup :: TableName             -> tables -> Maybe Relation
  insert :: TableName -> Relation -> tables -> tables

  populate :: [ Table ] -> tables
  populate = foldr (\ (tbl,rel) tables -> insert tbl rel tables) emptyTables


-- * Persistence
class (Tables tables, Monad m) => Persistable tables m where
  saveDB :: tables -> m ()
  loadDB :: m tables
