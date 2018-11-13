module KataPotterSpec where

import           Test.Hspec

spec :: Spec
spec = describe "Kata Potter" $ do
  it "pass the test" $
    True `shouldBe` True
