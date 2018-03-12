{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Sql.DB.Vector where

import qualified Data.Vector as Vector
import           Data.Word
import           Sql.DB

-- ** Binary DB

newtype Bytes = Bytes { bytes :: Vector.Vector Word8 }
  deriving (Eq,Show)

instance Serialize Relation  where
  put (Relation cols rws) = undefined
  get = undefined

instance DB Bytes where
  initDB                         = Bytes Vector.empty
  insert _tableName _relation db = db
  lookup _tableName _bytes       = Nothing
