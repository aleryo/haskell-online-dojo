module Sql.Relational(
    toRelational,
    Relational(..)
) where

import           Data.Text
import           Sql.DB
import           Sql.Parser

data Relational = Rel TableName
                | Proj [ ColumnName ] Relational
                | Prod [ Relational ]
                | Sel Expr Relational
                | Create TableName [ ColumnName ]
                | Append TableName Relation
  deriving (Eq, Show)

toRelational :: Sql -> Relational
toRelational (Select projs tableNames selection) =
    case selection of
        Nothing   -> Proj proj (relations tableNames)
        Just expr -> Proj proj (Sel expr (relations tableNames))
    where
        proj = [ x | Col x <- projs ]
        relations [ t ] = Rel t
        relations ts    = Prod $ fmap Rel ts

toRelational (Insert tableName cols values) =
    Append tableName (Relation cols (fmap (fmap eval) values))

toRelational (CreateTable tableName cols) =
    Create tableName cols

eval :: Expr -> Text
eval (Str s) = s
eval expr    = error $ "cannot eval " <> show expr
