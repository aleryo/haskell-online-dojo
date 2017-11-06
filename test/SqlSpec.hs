{-# LANGUAGE OverloadedStrings #-}
module SqlSpec where

import           Sql
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "SQL Parser" $ do

  it "can parse any pretty-printed valid SqlStatement" $ property $ canParsePrettyPrintedStatement

instance Arbitrary SqlStatement where
  arbitrary = Select <$> arbitrary

instance Arbitrary Expr where
  arbitrary = Number <$> arbitrary

canParsePrettyPrintedStatement :: SqlStatement -> Bool
canParsePrettyPrintedStatement statement =
  let sqlString = prettyPrint statement
  in  sqlParser sqlString == Right statement
