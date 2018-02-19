{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Sql.DB.Vector where

import Sql.DB
import qualified Data.Vector as Vector
import Data.Word

-- ** Binary DB

type Bytes = Vector.Vector Word8

instance DB Bytes where
