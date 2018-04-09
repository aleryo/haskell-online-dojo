{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Sql.DB.Vector where

import           Data.ByteString
import           Data.Monoid
import           Data.Serialize
import           Data.Text.Encoding
import           Sql.DB

-- ** Binary DB

newtype Bytes = Bytes { bytes :: ByteString }
  deriving (Eq,Show)

instance Serialize TableName where
  put = put . encodeUtf8
  get = decodeUtf8 <$> get

instance Serialize Relation  where
  put (Relation cols rws) = put (fmap encodeUtf8 cols) >> put (fmap (fmap encodeUtf8) rws)
  get = do
    cols <- fmap decodeUtf8 <$> get
    rws <- fmap (fmap decodeUtf8) <$> get
    pure $ Relation cols rws

instance DB Bytes where
  initDB                       = Bytes mempty
  insert tableName relation (Bytes db) =
      Bytes $ runPut (put tableName  >> put relation) <> db
  lookup tableName (Bytes db)    =
    let
      getLookup = do
        tbl <- get
        rel <- get
        if (tbl == tableName)
          then pure (Just rel)
          else getLookup

      reading = runGet getLookup db
    in
      case reading of
        Left _err -> Nothing
        Right t   -> t
