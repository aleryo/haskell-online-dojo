module KataPotterSpec where

import           Test.Hspec

spec :: Spec
spec = describe "Kata Potter" $ do
  --   2 copies of the first book
  -- 2 copies of the second book
  -- 2 copies of the third book
  -- 1 copy of the fourth book
  -- 1 copy of the fifth book
    --
    -- quel  est le prix ?
    -- il y a des reductions pour 2 vol -> 5 %, 3 vol -> 10 %, 4 vol -> 20 %, 5 vol -> 25 %
  it "pass the test" $
    True `shouldBe` True

