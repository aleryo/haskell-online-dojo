module AdventOfCode3Spec where

import Test.Hspec

data Vector = R Int | U Int | L Int | D Int deriving (Eq, Show)

data Position = Position { x::Int, y::Int } deriving (Eq, Show)

get_all_positions :: Position -> Vector -> [Position]
get_all_positions (Position x y) (U length) = reverse $ map (Position x) [ y  .. y + length] 
get_all_positions (Position x y) (D length) = map (Position x) [ y - length .. y ] 
get_all_positions (Position x y) (R length) = reverse $ map (`Position` y) [ x  .. x + length] 
get_all_positions (Position x y) (L length) = map (flip Position y) [ x - length .. x] 

accumulate_pos :: [Position] -> Vector -> [Position]
accumulate_pos acc@(initpos : rest) vector = (get_all_positions initpos vector) ++ rest

get_vectors_positions :: Position -> [Vector] -> [Position]
get_vectors_positions initialPos [] = [] 
get_vectors_positions initialPos vectors = reverse $ foldl accumulate_pos [initialPos] vectors

closest_crosswire_distance :: [ Vector ] ->  [ Vector ] -> Integer
closest_crosswire_distance = undefined

spec :: Spec
spec = describe "Crossed Wires" $ do

    -- it "first acceptance test" $ do
    --   closest_crosswire_distance [ R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72 ] [ U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83 ]
    --      `shouldBe` 159

    it "should return all position from a vector up" $ do
        get_all_positions (Position 0 0) (U 3) `shouldBe` reverse [Position 0 0, Position 0 1, Position 0 2, Position 0 3]

    it "should return all position from a vector down" $ do
        get_all_positions (Position 0 3) (D 3) `shouldBe` reverse [Position 0 3, Position 0 2, Position 0 1, Position 0 0]

    it "should return all position from a vector left" $ do
        get_all_positions (Position 3 0) (L 3) `shouldBe` reverse [Position 3 0, Position 2 0, Position 1 0, Position 0 0]

    it "should return all position from a vector right" $ do
        get_all_positions (Position 0 0) (R 3) `shouldBe` reverse [Position 0 0, Position 1 0, Position 2 0, Position 3 0]

    it "should return all positions from 2 vectors" $ do 
        get_vectors_positions (Position 0 0) [(U 3), (R 2)] `shouldBe` 
            [Position 0 0, Position 0 1, Position 0 2, Position 0 3, Position 1 3, Position 2 3]

    it "should return no positions from no vectors" $ do 
        get_vectors_positions (Position 0 0) [] `shouldBe` []