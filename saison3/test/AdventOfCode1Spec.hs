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

customAccumulator :: ([Integer], Integer) -> Integer -> ([Integer], Integer)
customAccumulator (xs, last) current = ((last + current) : xs, last + current)

existsTwice :: [Integer] -> Bool
existsTwice (x:xs) = x `elem` xs

explicit_recursive :: [Integer] -> [Integer] -> Integer
explicit_recursive _ (x:xs) | x `elem` xs  = x
explicit_recursive (q:qs) (x:xs) = explicit_recursive qs (q + x : x : xs) 

frequency_reached_twice_for :: [Integer] -> Integer
frequency_reached_twice_for input = explicit_recursive (cycle input) [0]

accumulated_frequency :: [Integer] -> [Integer]
accumulated_frequency = scanl (+) 0 

looping_accumulated_frequency :: [Integer] -> [Integer]
looping_accumulated_frequency input = accumulated_frequency (input <> input)

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

    it "frequency accumulation should work" $ do
        accumulated_frequency [3, 3, 4, -2, -4] `shouldBe` [0, 3, 6, 10, 8, 4 ]

    it "frequency accumulation should loop on initial list" $ do
        looping_accumulated_frequency [3, 3, 4, -2, -4] `shouldBe` [0, 3, 6, 10, 8, 4, 7, 10, 14, 12, 8]

    it "should find first duplicated frequency" $ do
        fmap  frequency_reached_twice_for (parseDay1Input "day1-input.txt") `shouldReturn` 413 
        