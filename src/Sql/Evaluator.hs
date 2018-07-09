{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Sql.Evaluator
  ( evaluate, Tables(..), evaluateDB, execDatabase, runDatabase, toRelational
  , Relational(..), Relation(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.List            (elemIndex)
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import           Prelude              hiding (lookup)
import           Sql.DB
import           Sql.Relational

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

evaluate :: (Tables db) => Relational -> db -> Either EvaluationError Relation
evaluate rel db = runDatabase db $ evaluateDB rel

execDatabase :: (Tables db) => db -> Database db a -> (Either EvaluationError a, db)
execDatabase db = flip runState db . runExceptT . tables

runDatabase :: (Tables db) => db -> Database db a -> Either EvaluationError a
runDatabase db = flip evalState db . runExceptT . tables

evaluateDB :: (Tables db) => Relational -> Database db Relation
evaluateDB (Rel tblName) =
  get >>= maybe  (throwError $ tableNotFound tblName) pure . lookup tblName

evaluateDB (Prod exprs) =
  foldM cartesianProduct (Relation [] [[]]) exprs
    where
      mergeRelations (Relation cs1 rs1) (Relation cs2 rs2) =
        Relation (cs1 <> cs2) [ t1 <> t2 | t1 <- rs1, t2 <- rs2 ]
      cartesianProduct rel expr = mergeRelations rel <$> evaluateDB expr

evaluateDB (Proj selected expr) = do
  Relation cols rws <- evaluateDB expr
  projection <- projectCols cols (Prelude.reverse selected)
  pure $ Relation selected (fmap projection rws)
  where
    projectCols :: [ ColumnName ] -> [ ColumnName ] -> Database db (Row -> Row)
    projectCols cols selectedCols = foldM project (const []) selectedCols
      where
        project f col =
          case col `elemIndex` cols of
            Just colNum -> pure $ \ row -> row !! colNum : f row
            Nothing     -> throwError $ columnNotFound col

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

evaluateDB Sel{} = undefined
