module TennisSpec where

import Test.Hspec

data PartieDeTennis = Initial 
                    | BMarque { resteDeLaPartie :: PartieDeTennis }
                    | AMarque { resteDeLaPartie :: PartieDeTennis }

data Point = Zero | Quinze | Trente | Quarante | Avantage
    deriving (Eq, Show)

increment :: Point -> Point
increment Zero = Quinze
increment Quinze = Trente
increment Trente = Quarante

type Jeu = Int

data Score = Score (Jeu, Jeu) (Point, Point) 
    deriving (Eq, Show)

scoreDeTennis :: PartieDeTennis -> Score
scoreDeTennis (AMarque partie)      = 
    case scoreDeTennis partie of
        Score jeux (Quarante, Avantage) -> Score jeux (Quarante, Quarante)
        Score jeux (Quarante, Quarante) -> Score jeux (Avantage, Quarante)
        Score (jeuA, jeuB) (Quarante, _) -> Score (jeuA + 1, jeuB) (Zero, Zero)
        Score jeux (scoreDeA, scoreDeB) -> Score jeux (increment scoreDeA, scoreDeB)
scoreDeTennis (BMarque partie)      = 
    case scoreDeTennis partie of
        Score jeux (Avantage, Quarante) -> Score jeux (Quarante, Quarante) 
        Score jeux (Quarante, Quarante) -> Score jeux (Quarante, Avantage)
        Score (jeuA, jeuB) (_, Quarante) -> Score (jeuA, jeuB + 1) (Zero, Zero)
        Score jeux (scoreDeA, scoreDeB) -> Score jeux (scoreDeA, increment scoreDeB)
scoreDeTennis Initial              = Score (0, 0) (Zero, Zero)

spec :: Spec
spec = describe "score de tennis" $ do
    it "score initial est 0-0" $ do
        scoreDeTennis Initial `shouldBe` Score (0,0) (Zero, Zero)
    it "score est 15-0 lorsque A marque" $
        scoreDeTennis (AMarque Initial) `shouldBe` Score (0, 0) (Quinze, Zero)
    it "score est 0-15 lorsque B marque" $
        scoreDeTennis (BMarque Initial) `shouldBe` Score (0, 0) (Zero, Quinze)
    it "score est 30-0 lorsque A marque 2 fois" $
        scoreDeTennis (AMarque $ AMarque Initial) `shouldBe` Score (0, 0) (Trente, Zero)
    it "score est 0-30 lorsque B marque 2 fois" $
        scoreDeTennis (BMarque $ BMarque Initial) `shouldBe` Score (0, 0) (Zero, Trente)
    it "score est 15-15 lorsque A et B marquent" $
        scoreDeTennis (AMarque $ BMarque Initial) `shouldBe` Score (0, 0) (Quinze, Quinze)
    it "score est 40-40 lorsque A et B marquent chacun 3 fois" $
        scoreDeTennis (AMarque $ AMarque $ AMarque $ BMarque $ BMarque $ BMarque Initial)
            `shouldBe` Score (0, 0) (Quarante, Quarante)
    it "Si le score est de 40-40 lorsque A marque il prend l'avantage" $
        scoreDeTennis (AMarque $ AMarque $ AMarque $ AMarque $ BMarque $ BMarque $ BMarque Initial)
            `shouldBe` Score (0, 0) (Avantage, Quarante)
    it "Si A à l'avantage et que B marque le score est de 40-40" $
        scoreDeTennis (BMarque $ AMarque $ AMarque $ AMarque $ AMarque $ BMarque $ BMarque $ BMarque Initial)
            `shouldBe` Score (0, 0) (Quarante, Quarante)
    it "Si B à l'avantage et que A marque le score est de 40-40" $
        scoreDeTennis (AMarque $ BMarque $ AMarque $ AMarque $ AMarque $ BMarque $ BMarque $ BMarque Initial)
            `shouldBe` Score (0, 0) (Quarante, Quarante)
    it "score est 1-0 sur le jeu précédent et 0-0 sur le jeu suivant lorsque A marque 4 fois" $
        scoreDeTennis (AMarque $ AMarque $ AMarque $ AMarque Initial) `shouldBe` Score (1, 0) (Zero, Zero)
    it "score est 0-1 sur le jeu précédent et 0-0 sur le jeu suivant lorsque B marque 4 fois" $
        scoreDeTennis (BMarque $ BMarque $ BMarque $ BMarque Initial) `shouldBe` Score (0, 1) (Zero, Zero)