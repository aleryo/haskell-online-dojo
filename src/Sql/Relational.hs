module Sql.Relational(
    toRelational,
    Relational(..)
) where

import Data.Monoid
import Data.Text
import Sql.Parser
import Sql.DB

data Relational = Rel TableName
                | Proj [ ColumnName ] Relational
                | Prod [ Relational ]
                | Create TableName [ ColumnName ]
                | Append TableName Relation
  deriving (Eq, Show)

toRelational :: Sql -> Relational
toRelational (Select projs tableNames Nothing) =
    Proj proj (relations tableNames)
    where
        proj = [ x | Col x <- projs ]
        relations [ t ] = Rel t
        relations ts    = Prod $ fmap Rel ts
toRelational Select{} = undefined

toRelational (Insert tableName cols values) =
    Append tableName (Relation cols (fmap (fmap eval) values))

toRelational (CreateTable tableName cols) =
    Create tableName cols

eval :: Expr -> Text
eval (Str s) = s
eval expr    = error $ "cannot eval " <> show expr