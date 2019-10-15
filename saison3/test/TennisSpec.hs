module TennisSpec where

import Test.Hspec

data PartieDeTennis = Initial 
                    | BMarque { resteDeLaPartie :: PartieDeTennis }
                    | AMarque { resteDeLaPartie :: PartieDeTennis }
    deriving (Eq, Show)

data Point = Zero | Quinze | Trente | Quarante | Avantage
    deriving (Eq, Show)

data FeuilleDeMatch = FeuilleDeMatch { sets :: (Set, Set)
                                     , partie :: PartieDeTennis }
    deriving (Eq, Show)

increment :: Point -> Point
increment Zero = Quinze
increment Quinze = Trente
increment Trente = Quarante

type Jeu = Int
type Set = Int

data Score = Score (Set, Set) (Jeu, Jeu) (Point, Point) 
    deriving (Eq, Show)

scoreDeTennis :: PartieDeTennis -> Score
scoreDeTennis (AMarque partie)      = 
    case scoreDeTennis partie of
        Score sets jeux (Quarante, Avantage) -> Score sets jeux (Quarante, Quarante)
        Score sets jeux (Quarante, Quarante) -> Score sets jeux (Avantage, Quarante)
        Score (setA, setB) (5, _) (Quarante, _) -> Score (setA + 1, setB) (0, 0) (Zero, Zero)
        Score sets (jeuA, jeuB) (Quarante, _) -> Score sets (jeuA + 1, jeuB) (Zero, Zero)
        Score sets jeux (scoreDeA, scoreDeB) -> Score sets jeux (increment scoreDeA, scoreDeB)
scoreDeTennis (BMarque partie)      = 
    case scoreDeTennis partie of
        Score sets jeux (Avantage, Quarante) -> Score sets jeux (Quarante, Quarante) 
        Score sets jeux (Quarante, Quarante) -> Score sets jeux (Quarante, Avantage)
        Score (setA, setB) (_, 5) (_, Quarante) -> Score (setA, setB + 1) (0, 0) (Zero, Zero)
        Score sets (jeuA, jeuB) (_, Quarante) -> Score sets (jeuA, jeuB + 1) (Zero, Zero)
        Score sets jeux (scoreDeA, scoreDeB) -> Score sets jeux (scoreDeA, increment scoreDeB)
scoreDeTennis Initial              = Score (0, 0) (0, 0) (Zero, Zero)

feuilleDeMatch :: PartieDeTennis -> Maybe FeuilleDeMatch
feuilleDeMatch Initial = Nothing
feuilleDeMatch partie = case scoreDeTennis partie of
    Score sets (0, 0) (Zero, Zero) -> Just $ FeuilleDeMatch sets partie

spec :: Spec
spec = describe "score de tennis" $ do
    it "score initial est 0-0" $ do
        scoreDeTennis Initial `shouldBe` Score (0,0) (0,0) (Zero, Zero)
    it "score est 15-0 lorsque A marque" $
        scoreDeTennis (AMarque Initial) `shouldBe` Score (0, 0) (0, 0) (Quinze, Zero)
    it "score est 0-15 lorsque B marque" $
        scoreDeTennis (BMarque Initial) `shouldBe` Score (0, 0) (0, 0) (Zero, Quinze)
    it "score est 30-0 lorsque A marque 2 fois" $
        scoreDeTennis (AMarque $ AMarque Initial) `shouldBe` Score (0, 0) (0, 0) (Trente, Zero)
    it "score est 0-30 lorsque B marque 2 fois" $
        scoreDeTennis (BMarque $ BMarque Initial) `shouldBe` Score (0, 0) (0, 0) (Zero, Trente)
    it "score est 15-15 lorsque A et B marquent" $
        scoreDeTennis (AMarque $ BMarque Initial) `shouldBe` Score (0, 0) (0, 0) (Quinze, Quinze)
    it "score est 40-40 lorsque A et B marquent chacun 3 fois" $
        scoreDeTennis (AMarque $ AMarque $ AMarque $ BMarque $ BMarque $ BMarque Initial)
            `shouldBe` Score (0, 0) (0, 0) (Quarante, Quarante)
    it "Si le score est de 40-40 lorsque A marque il prend l'avantage" $
        scoreDeTennis (AMarque $ AMarque $ AMarque $ AMarque $ BMarque $ BMarque $ BMarque Initial)
            `shouldBe` Score (0, 0) (0, 0) (Avantage, Quarante)
    it "Si A à l'avantage et que B marque le score est de 40-40" $
        scoreDeTennis (BMarque $ AMarque $ AMarque $ AMarque $ AMarque $ BMarque $ BMarque $ BMarque Initial)
            `shouldBe` Score (0, 0) (0, 0) (Quarante, Quarante)
    it "Si B à l'avantage et que A marque le score est de 40-40" $
        scoreDeTennis (AMarque $ BMarque $ AMarque $ AMarque $ AMarque $ BMarque $ BMarque $ BMarque Initial)
            `shouldBe` Score (0, 0) (0, 0) (Quarante, Quarante)
    it "score est 1-0 sur le jeu précédent et 0-0 sur le jeu suivant lorsque A marque 4 fois" $
        scoreDeTennis (AMarque $ AMarque $ AMarque $ AMarque Initial) `shouldBe` Score (0, 0)  (1, 0) (Zero, Zero)
    it "score est 0-1 sur le jeu précédent et 0-0 sur le jeu suivant lorsque B marque 4 fois" $
        scoreDeTennis (BMarque $ BMarque $ BMarque $ BMarque Initial) `shouldBe` Score (0, 0) (0, 1) (Zero, Zero)
    it "score est de 0 set a 1 lorsque B marque 24 fois" $
        scoreDeTennis (foldr ($) Initial (replicate 24 BMarque)) `shouldBe` Score (0, 1) (0, 0) (Zero, Zero)
    it "score est de 1 set a 0 lorsque A marque 24 fois" $
        scoreDeTennis (foldr ($) Initial (replicate 24 AMarque)) `shouldBe` Score (1, 0) (0, 0) (Zero, Zero)
    it "score est de 0 set a 0 lorsque A marque 23 fois suivi de 3 BMarque" $
        scoreDeTennis (AMarque $ BMarque $ BMarque $ BMarque $ foldr ($) Initial (replicate 23 AMarque)) `shouldBe` Score (0, 0) (5, 0) (Avantage, Quarante)
    
    describe "calcul de la fin de la partie" $ do
        it "pas de fin de partie pour une partie Initial" $
            feuilleDeMatch Initial `shouldBe` Nothing
        it "feuille de match lorsque A fait un perfect" $
            sets <$> (feuilleDeMatch (foldr ($) Initial (replicate 72 AMarque))) `shouldBe` Just (3, 0)
        it "pas de feuille de match si 2 jeux gagnes" $
            sets <$> (feuilleDeMatch (foldr ($) Initial (replicate 48 AMarque))) `shouldBe` Nothing
