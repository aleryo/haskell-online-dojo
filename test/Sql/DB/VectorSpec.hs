{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Sql.DB.VectorSpec where

import           Data.Monoid     ((<>))
import           Data.Text       (Text, pack)
import qualified Data.Vector     as Vector
import           Sql.DB          as DB
import           Sql.DB.Vector
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "Binary Representation of DB" $ do

  it "initialise empty vector" $ do
    bytes initDB `shouldBe` Vector.empty

  it "can lookup inserted relations" $ property $ prop_canLookupInsertedRelations

newtype TblName = TblName Text
  deriving (Eq, Show)

instance Arbitrary TblName where
  arbitrary = TblName <$> genTableName

genTableName :: Gen Text
genTableName = do
  Positive n <- arbitrary
  pure $ pack $ "table-" <> show (n :: Int)

instance Arbitrary Relation where
  arbitrary = do
    cols <- genColumns
    rs <- genRows cols
    pure $ Relation cols rs

genColumns :: Gen [ Text ]
genColumns = listOf genColumn

genColumn :: Gen Text
genColumn = do
  Positive n <- arbitrary
  pure $ pack $ "col-" <> show (n :: Int)


genRows :: [Text] -> Gen [[Text]]
genRows (length -> numCol) =
  listOf (genLines numCol)

genLines :: Int -> Gen [Text]
genLines numCol = vectorOf numCol genData

genData :: Gen Text
genData = pack <$> listOf (choose ('a', 'z'))

prop_canLookupInsertedRelations :: [ (TblName, Relation) ] -> Bool
prop_canLookupInsertedRelations tables =
  let
    finalDB = foldr (\ (TblName n,r) db ->  insert n r db) (initDB :: Bytes) tables
  in
    all ( \(TblName tname, rel) -> DB.lookup tname finalDB == Just rel) tables
