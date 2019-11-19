module AdventOfCode1Spec where

import Test.Hspec


parseDay1Input :: [Char] -> IO [Integer]
parseDay1Input file = do
    lines <- parseLines file
    return $ fmap parseLine lines

parseLines :: String -> IO [String] 
parseLines file = lines <$> (readFile file)


parseLine  :: String -> Integer
parseLine ('+':string) = read string
parseLine s = read s

frequency :: [Integer] -> Integer
frequency = sum

frequency_reached_twice_for :: [Integer] -> Integer
frequency_reached_twice_for = const 0 

spec :: Spec
spec = describe "Chronal Calibration" $ do

    it "should read one line" $ do
        fmap head (parseDay1Input "day1-input.txt") `shouldReturn` 13 
 
    it "should read 5th line" $ do
        fmap (head . drop 4) (parseDay1Input "day1-input.txt") `shouldReturn` -15 
    
    it "should be 493" $ do
        fmap frequency (parseDay1Input "day1-input.txt") `shouldReturn` 493

    it "frequency reached twice for [+1, -1] is 0" $ do
        frequency_reached_twice_for [1, -1] `shouldBe` 0
        frequency_reached_twice_for [3, 3, 4, -2, -4] `shouldBe` 10