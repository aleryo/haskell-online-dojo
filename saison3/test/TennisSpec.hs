module TennisSpec where

import Test.Hspec

data PartieDeTennis = Initial 
                    | BMarque { resteDeLaPartie :: PartieDeTennis }
                    | AMarque { resteDeLaPartie :: PartieDeTennis }

data Point = Zero | Quinze | Trente
    deriving (Eq, Show)

increment :: Point -> Point
increment Zero = Quinze
increment Quinze = Trente
increment Trente = undefined

initial :: PartieDeTennis
initial = Initial

aMarque :: PartieDeTennis -> PartieDeTennis
aMarque = AMarque 

bMarque :: PartieDeTennis -> PartieDeTennis
bMarque = BMarque

type Score = (Point, Point)

scoreDeTennis :: PartieDeTennis -> Score
scoreDeTennis (AMarque partie)      = 
    let (scoreDeA, scoreDeB) = scoreDeTennis partie 
    in (increment scoreDeA, scoreDeB)
scoreDeTennis (BMarque partie)      = 
    let (scoreDeA, scoreDeB) = scoreDeTennis partie 
    in (scoreDeA, increment scoreDeB)
scoreDeTennis Initial              = (Zero, Zero)

spec :: Spec
spec = describe "score de tennis" $ do
    it "score initial est 0-0" $ do
        scoreDeTennis initial `shouldBe` (Zero, Zero)
    it "score est 15-0 lorsque A marque" $
        scoreDeTennis (aMarque initial) `shouldBe` (Quinze, Zero)
    it "score est 0-15 lorsque B marque" $
        scoreDeTennis (bMarque initial) `shouldBe` (Zero, Quinze)
    it "score est 30-0 lorsque A marque 2 fois" $
        scoreDeTennis (aMarque $ aMarque initial) `shouldBe` (Trente, Zero)
    it "score est 0-30 lorsque B marque 2 fois" $
        scoreDeTennis (bMarque $ bMarque initial) `shouldBe` (Zero, Trente)
    it "score est 15-15 lorsque A et B marquent" $
        scoreDeTennis (aMarque $ bMarque initial) `shouldBe` (Quinze, Quinze)