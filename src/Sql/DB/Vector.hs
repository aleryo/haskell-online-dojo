{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Sql.DB.Vector where

import qualified Data.Vector as Vector
import           Data.Word
import           Sql.DB

-- ** Binary DB

newtype Bytes = Bytes { bytes :: Vector.Vector Word8 }
  deriving (Eq,Show)

instance DB Bytes where
  initDB                             = Bytes Vector.empty
  lookup _tableName _bytes           = undefined
  insert _tableName _relation _bytes = undefined
