{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Sql.DB.VectorDBSpec where

import           Data.Monoid
import           Data.Serialize
import           Data.Text               (Text, pack)
import           Sql.DB                  as DB
import           Sql.DB.VectorDB
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

spec :: Spec
spec = describe "Binary Representation of DB" $ do

  it "initialise empty vector" $ do
    bytes emptyTables `shouldBe` mempty

  it "can seriaze relations" $ withMaxSuccess 30 $ property $ prop_canRoundtripRelationSerialization
  it "can lookup inserted relations" $ withMaxSuccess 30 $ property $ prop_canLookupInsertedRelations
  it "can load/store from file" $ withMaxSuccess 30 $ property $ prop_canStoreAndLoadAVectorDB


prop_canRoundtripRelationSerialization :: Relation -> Bool
prop_canRoundtripRelationSerialization rel =
  runGet get (runPut (put rel)) == Right rel

prop_canLookupInsertedRelations :: [ Relation ] -> Bool
prop_canLookupInsertedRelations rels =
  let
    tables = zip mkTableNames rels
    insert' (TblName n, r) db = insert n r db
    finalDB = foldr insert' (emptyTables :: BytesDB) tables
  in
    all ( \(TblName tname, rel) -> DB.lookup tname finalDB == Just rel) tables

prop_canStoreAndLoadAVectorDB :: Relation -> Property
prop_canStoreAndLoadAVectorDB rel = monadicIO $ do
  let vectorDB = insert "table-1" rel (emptyTables :: BytesDB)
  db <- run $ do
    saveDB vectorDB
    loadDB

  assert $ db == vectorDB

newtype TblName = TblName Text
  deriving (Eq, Show)

mkTableNames :: [ TblName ]
mkTableNames = fmap ( \ n -> TblName $ pack $ "table-" <> show (n :: Int)) [ 1 .. ]

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
