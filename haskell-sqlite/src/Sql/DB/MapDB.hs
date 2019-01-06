{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Sql.DB.MapDB where

import qualified Data.Map as Map
import           Sql.DB

-- ** Naive Implementation as Map

type MapDB = Map.Map TableName Relation

instance Tables MapDB where
  lookup = Map.lookup
  insert = Map.insert
  emptyTables = Map.empty
