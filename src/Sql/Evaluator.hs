{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Sql.Evaluator
  ( evaluate, DB(..), evaluateDB, execDatabase, runDatabase, toRelational
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

evaluateDB (Prod exprs) =
  foldM cartesianProduct (Relation [] [[]]) exprs
    where
      mergeRelations (Relation cs1 rs1) (Relation cs2 rs2) =
        Relation (cs1 <> cs2) [ t1 <> t2 | t1 <- rs1, t2 <- rs2 ]
      cartesianProduct rel expr = mergeRelations rel <$> evaluateDB expr

evaluateDB (Proj selected expr) = do
  Relation cols rws <- evaluateDB expr
  projection <- projectCols (Prelude.reverse selected) cols
  pure $ Relation selected (fmap projection rws)
  where
    projectCols :: [ ColumnName ] -> [ ColumnName ] -> Database db (Row -> Row)
    projectCols selectedCols cols = foldM project (const []) selectedCols
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
