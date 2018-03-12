module Sql.DB where

import           Data.Text (Text)

type TableName = Text

type ColumnName = Text

data Relation = Relation { columnNames :: [ Text ]
                         , rows        :: [[ Text ]]
                         }
                deriving (Eq, Show)


class DB db where
  initDB :: db
  lookup :: TableName             -> db -> Maybe Relation
  insert :: TableName -> Relation -> db -> db

populate :: (DB db) => [ (TableName, Relation) ] -> db
populate = foldr (\ (tbl,rel) db -> insert tbl rel db) initDB
