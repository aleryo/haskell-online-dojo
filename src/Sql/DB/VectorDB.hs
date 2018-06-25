{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Sql.DB.VectorDB where

import           Data.ByteString    as BS
import           Data.Monoid
import           Data.Serialize
import           Data.Text.Encoding
import           Sql.DB

-- ** Binary DB

newtype BytesDB = BytesDB { bytes :: ByteString }
  deriving (Eq,Show)

makeBytesDB :: (Monad m) => ByteString -> m BytesDB
makeBytesDB = pure . BytesDB

instance Persistable BytesDB IO where
  loadDB = BS.readFile "./sqlite.db" >>= makeBytesDB
  saveDB BytesDB { bytes } = BS.writeFile "./sqlite.db" bytes


instance Serialize TableName where
  put = put . encodeUtf8
  get = decodeUtf8 <$> get

instance Serialize Relation  where
  put (Relation cols rws) = put (fmap encodeUtf8 cols) >> put (fmap (fmap encodeUtf8) rws)
  get = do
    cols <- fmap decodeUtf8 <$> get
    rws <- fmap (fmap decodeUtf8) <$> get
    pure $ Relation cols rws

instance Tables BytesDB where
  initDB                       = BytesDB mempty
  insert tableName relation (BytesDB db) =
    BytesDB $ runPut (put tableName  >> put relation) <> db
  lookup tableName (BytesDB db)    =
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
