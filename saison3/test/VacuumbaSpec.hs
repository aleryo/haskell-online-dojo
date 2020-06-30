{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module VacuumbaSpec where

import Test.Hspec
import Test.QuickCheck

-- https://open.kattis.com/problems/vacuumba
spec :: Spec
spec = describe "Vacuumba" $ do
    it "should move upward" $ property $ prop_moves_upward_given_angle_is_0        
    it "should rotate" $ property $ prop_rotates_position_given_a_move        

newtype Distance = Distance { distance :: Double } 
    deriving (Eq, Show, Num)

instance Arbitrary Distance where
    arbitrary = Distance <$> choose (-100, 100)

newtype Angle = Angle { angle :: Double } 
    deriving (Eq, Show, Num)

instance Arbitrary Angle where
    arbitrary = Angle <$> choose (-360, 360)

newtype Move = Move (Angle, Distance)
    deriving (Eq, Show)

instance Arbitrary Move where
    arbitrary = do
        a <- arbitrary
        d <- arbitrary
        return (Move (a, d))

prop_moves_upward_given_angle_is_0 :: Move -> Bool
prop_moves_upward_given_angle_is_0 (Move (_, d)) =
    move (Move (0, d)) == (0, distance d)

prop_rotates_position_given_a_move :: Move -> Property
prop_rotates_position_given_a_move m@(Move(_, d)) = 
    let target = move m
        dist = distanceL2 target (0,0)
    in counterexample ("target is " ++ show target ++", distance is " ++ show dist) $ dist ==  abs d

move :: Move  ->  (Double, Double)
move (Move (Angle 0 , Distance d)) = (0, d)
move (Move (Angle a, Distance d)) = (d * cos r, d * sin r)
    where r = a/180*pi


distanceL2 :: (Double, Double) -> (Double, Double) -> Distance
distanceL2 (x1, y1) _ = Distance $ sqrt (x1 * x1 + y1*y1)