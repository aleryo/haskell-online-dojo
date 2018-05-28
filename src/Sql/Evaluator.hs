{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Sql.Evaluator
  ( evaluate, DB(..), populate, evaluateDB, execDatabase, runDatabase, toRelational
  , Relational(..), Relation(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.List            (elemIndex)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            hiding (foldr)
import           Prelude              hiding (lookup)
import           Sql.DB
import           Sql.Parser           (Expr (..), Sql (..))

data Relational = Rel TableName
                | Proj [ ColumnName ] Relational
                | Prod [ Relational ]
                | Create TableName Relation
                | Create' TableName [ ColumnName ]
                -- ^TODO: refactor, added to not break too many existing tests
                | Add TableName Relation
  deriving (Eq, Show)


type EvaluationError = Text

relationNotFound :: TableName -> EvaluationError
relationNotFound name = "no relation with name " <> (pack $ show name)

columnNotFound :: ColumnName -> EvaluationError
columnNotFound name = "no column with name " <> (pack $ show name)

newtype Database db a = Database { tables :: ExceptT EvaluationError (State db) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState db
           , MonadError EvaluationError
           )

evaluate :: (DB db) => Relational -> db -> Either EvaluationError Relation
evaluate rel db = runDatabase db $ evaluateDB rel

execDatabase :: (DB db) => db -> Database db a -> (Either EvaluationError a, db)
execDatabase db = flip runState db . runExceptT . tables

runDatabase :: (DB db) => db -> Database db a -> Either EvaluationError a
runDatabase db = flip evalState db . runExceptT . tables

evaluateDB :: (DB db) => Relational -> Database db Relation
evaluateDB (Rel tblName) =
  get >>= maybe  (throwError $ relationNotFound tblName) pure . lookup tblName

evaluateDB (Prod [rel1,rel2]) = do
  table1 <- evaluateDB rel1
  table2 <- evaluateDB rel2
  return $ Relation (columnNames table1 <> columnNames table2) [ t1 <> t2 | t1 <- rows table1, t2 <- rows table2 ]

evaluateDB (Proj [col] rel) = do
  Relation cols rws <- evaluateDB rel
  case elemIndex col cols of
    Just colNum ->
      let projectCols row = [ row !! colNum ]
      in  pure $ Relation [ col ] (fmap projectCols rws)
    Nothing -> throwError $ columnNotFound col

evaluateDB (Create tbl rel)  = do
  modify $ insert tbl rel
  return rel

evaluateDB (Create' tbl cols)  = do
  let rel = Relation cols []
  modify $ insert tbl rel
  return rel

evaluateDB (Add tbl rel)  = do
  db <- get
  case lookup tbl db of
    Nothing -> throwError $ relationNotFound tbl
    Just rel' ->
      if columnNames rel' == columnNames rel
      then put (insert tbl (Relation (columnNames rel) (rows rel' <> rows rel)) db)
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
toRelational (CreateTable tableName cols) =
   Create tableName (Relation cols [])

eval :: Expr -> Text
eval (Str s) = s
eval expr    = error $ "cannot eval " <> show expr
