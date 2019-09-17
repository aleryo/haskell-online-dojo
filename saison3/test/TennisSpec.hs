module TennisSpec where

import Test.Hspec

data PartieDeTennis = Initial | AMarque

data Point = Zero | Quinze
    deriving (Eq, Show)

initial :: PartieDeTennis
initial = Initial

aMarque :: PartieDeTennis
aMarque = AMarque

type Score = (Point, Point)

scoreDeTennis :: PartieDeTennis -> Score
scoreDeTennis AMarque = (Quinze, Zero)
scoreDeTennis _ = (Zero, Zero)

spec :: Spec
spec = describe "score de tennis" $ do
    it "score initial est 0-0" $ do
        scoreDeTennis initial `shouldBe` (Zero, Zero)
    it "score est 15-0 lorsque A marque" $
        scoreDeTennis (aMarque) `shouldBe` (Quinze, Zero)