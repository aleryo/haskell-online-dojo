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
                | Create TableName [ ColumnName ]
                | Append TableName Relation
  deriving (Eq, Show)


type EvaluationError = Text

tableNotFound :: TableName -> EvaluationError
tableNotFound name = "no table with name " <> (pack $ show name)

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
  get >>= maybe  (throwError $ tableNotFound tblName) pure . lookup tblName

evaluateDB (Prod [expr1,expr2]) = do
  relation1 <- evaluateDB expr1
  relation2 <- evaluateDB expr2
  return $ Relation (columnNames relation1 <> columnNames relation2) [ t1 <> t2 | t1 <- rows relation1, t2 <- rows relation2 ]

evaluateDB (Prod [expr1,expr2,expr3]) = do
  relation1 <- evaluateDB expr1
  relation2 <- evaluateDB expr2
  relation3 <- evaluateDB expr3
  return $ Relation (columnNames relation1 <> columnNames relation2 <> columnNames relation3) [ t1 <> t2 <> t3 | t1 <- rows relation1, t2 <- rows relation2, t3 <- rows relation3 ]

evaluateDB (Proj [col] expr) = do
  Relation cols rws <- evaluateDB expr
  case elemIndex col cols of
    Just colNum ->
      let projectCols row = [ row !! colNum ]
      in  pure $ Relation [ col ] (fmap projectCols rws)
    Nothing -> throwError $ columnNotFound col

evaluateDB (Create tbl cols)  = do
  let rel = Relation cols []
  modify $ insert tbl rel
  return rel

evaluateDB (Append tbl rel)  = do
  db <- get
  case lookup tbl db of
    Nothing -> throwError $ tableNotFound tbl
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
   Append tableName (Relation cols (fmap (fmap eval) values))
toRelational (CreateTable tableName cols) =
   Create tableName cols

eval :: Expr -> Text
eval (Str s) = s
eval expr    = error $ "cannot eval " <> show expr
