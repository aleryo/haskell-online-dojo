module KataPotterSpec where

import           Test.Hspec

data Book = Book Int
  deriving (Eq)

totalPrice [x, y, z] = (8 + 8 + 8) * 0.90
totalPrice [x,y]
  | x /= y = (8 + 8) * 0.95
  | otherwise = 8 + totalPrice [y]
totalPrice [_]     = 8

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
  it "buys one book yields 0 discount" $
    totalPrice [ Book 1 ] `shouldBe` 8

  it "buys two books yields 5% discount" $ do
    totalPrice [ Book 1, Book 2  ] `shouldBe` (8 + 8) * 0.95
    totalPrice [ Book 1, Book 1 ] `shouldBe` (8 + 8)

  it "buys three books yields 10% discount" $ do
    totalPrice [ Book 1, Book 2, Book 3  ] `shouldBe` (8 + 8 + 8) * 0.90
