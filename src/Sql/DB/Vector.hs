{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Sql.DB.Vector where

import           Data.Serialize
import           Data.Text.Encoding
import qualified Data.Vector        as Vector
import           Data.Word
import           Sql.DB

-- ** Binary DB

newtype Bytes = Bytes { bytes :: Vector.Vector Word8 }
  deriving (Eq,Show)

instance Serialize Relation  where
  put (Relation cols rws) = put (fmap encodeUtf8 cols) >> put (fmap (fmap encodeUtf8) rws)
  get = do
    cols <- fmap decodeUtf8 <$> get
    rws <- fmap (fmap decodeUtf8) <$> get
    pure $ Relation cols rws

instance DB Bytes where
  initDB                         = Bytes Vector.empty
  insert _tableName _relation db = db
  lookup _tableName _bytes       = Nothing
