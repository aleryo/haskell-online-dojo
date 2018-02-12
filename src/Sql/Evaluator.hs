 {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Sql.Evaluator
  ( evaluate, populate, evaluateDB, execDatabase, runDatabase, toRelational
  , Relational(..), Relation(..), DB
  ) where

import Data.Maybe
import Sql.Parser(Sql(..), Expr(..))
import Control.Monad.State
import Control.Monad.Except
import Data.Monoid
import Data.Text
import Data.List(elemIndex)
import qualified Data.Map as Map


type TableName = Text

type ColumnName = Text

data Relational = Rel TableName
                | Proj [ ColumnName ] Relational
                | Prod [ Relational ]
                | Create TableName Relation
                | Create' TableName [ ColumnName ]
                -- ^TODO: refactor, added to not break too many existing tests
                | Add TableName Relation
  deriving (Eq, Show)


data Relation = Relation { columnNames :: [ Text ]
                         , rows       :: [[ Text ]]
                         }
                deriving (Eq, Show)

type EvaluationError = Text

relationNotFound :: TableName -> EvaluationError
relationNotFound name = "no relation with name " <> (pack $ show name)

type DB = Map.Map TableName Relation

newtype Database a = Database { tables :: ExceptT EvaluationError (State DB) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState DB
           , MonadError EvaluationError
           )

populate :: [ (TableName, Relation) ] -> DB
populate = Map.fromList

evaluate :: Relational -> DB -> Either EvaluationError Relation
evaluate rel db = runDatabase db $ evaluateDB rel

execDatabase :: DB -> Database a -> (Either EvaluationError a, DB)
execDatabase db = flip runState db . runExceptT . tables

runDatabase :: DB -> Database a -> Either EvaluationError a
runDatabase db = flip evalState db . runExceptT . tables

evaluateDB :: Relational -> Database Relation
evaluateDB (Rel tblName) =
  get >>= maybe  (throwError $ relationNotFound tblName) pure . Map.lookup tblName

evaluateDB (Prod [rel1,rel2]) = do
  table1 <- evaluateDB rel1
  table2 <- evaluateDB rel2
  return $ Relation (columnNames table1 <> columnNames table2) [ t1 <> t2 | t1 <- rows table1, t2 <- rows table2 ]

evaluateDB (Proj [col] rel) = do
  Relation cols rws <- evaluateDB rel
  let
    projectCols row = [ row !! colNum ]
    colNum = fromJust $ elemIndex col cols
  pure $ Relation [ col ] (fmap projectCols rws)

evaluateDB (Create tbl rel)  = do
  modify $ Map.insert tbl rel
  return rel

evaluateDB (Create' tbl cols)  = do
  let rel = Relation cols []
  modify $ Map.insert tbl rel
  return rel

evaluateDB (Add tbl rel)  = do
  db <- get
  case Map.lookup tbl db of
    Nothing -> throwError $ relationNotFound tbl
    Just rel' ->
      if columnNames rel' == columnNames rel
      then put (Map.insert tbl (Relation (columnNames rel) (rows rel' <> rows rel)) db)
      else throwError "Incompatible relation schemas"
  return rel

evaluateDB expr  = throwError $ "Don't know how to evaluate " <> pack (show expr)

toRelational :: Sql -> Relational
toRelational (Select projs tableNames) =
   Proj proj (relations tableNames)
   where
     proj = [ x | Col x <- projs ]
     relations [ t ] = Rel t
     relations ts    = Prod $ fmap Rel ts
toRelational (Insert tableName cols values) =
   Create tableName (Relation cols (fmap (fmap eval) values))

eval :: Expr -> Text
eval (Str s) = s
eval expr    = error $ "cannot eval " <> show expr
