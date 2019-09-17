module TennisSpec where

import Test.Hspec

data PartieDeTennis = Initial 
                    | BMarque
                    | AMarque { resteDeLaPartie :: PartieDeTennis }

data Point = Zero | Quinze | Trente
    deriving (Eq, Show)

initial :: PartieDeTennis
initial = Initial

aMarque :: PartieDeTennis -> PartieDeTennis
aMarque = AMarque 

bMarque :: PartieDeTennis
bMarque = BMarque

type Score = (Point, Point)

scoreDeTennis :: PartieDeTennis -> Score
scoreDeTennis (AMarque (AMarque _)) = (Trente, Zero)
scoreDeTennis (AMarque _)           = (Quinze, Zero)
scoreDeTennis BMarque               = (Zero, Quinze)
scoreDeTennis _                     = (Zero, Zero)

spec :: Spec
spec = describe "score de tennis" $ do
    it "score initial est 0-0" $ do
        scoreDeTennis initial `shouldBe` (Zero, Zero)
    it "score est 15-0 lorsque A marque" $
        scoreDeTennis (aMarque initial) `shouldBe` (Quinze, Zero)
    it "score est 0-15 lorsque B marque" $
        scoreDeTennis (bMarque) `shouldBe` (Zero, Quinze)
    it "score est 30-0 lorsque A marque 2 fois" $
        scoreDeTennis (aMarque (aMarque initial)) `shouldBe` (Trente, Zero)