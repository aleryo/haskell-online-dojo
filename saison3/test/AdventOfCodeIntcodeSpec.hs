module AdventOfCodeIntcodeSpec where

import Test.Hspec
import Data.List.Split

newtype Cursor = Cursor Int

newtype Interpreter = Interpreter ([Int], Cursor)

memory :: Interpreter -> [Int]
memory (Interpreter (memory, _)) = memory

cursor :: Interpreter -> Int
cursor (Interpreter (_, Cursor c)) = c

start :: [Int] -> Interpreter
start startingMemory = Interpreter (startingMemory, Cursor 0)

interpret :: [Int] -> [Int]
interpret = memory . interpret' . start

interpretReprogrammed :: [Int] -> Integer -> Integer -> [Int]
---nterpretReprogrammed interpreter  _ _  = memory . interpret' . start $ interpreter
interpretReprogrammed interpreter  a b  = memory . interpret' . (put (put (start interpreter) 1 12 ) 2 23)

currentInstruction :: Interpreter -> Int
currentInstruction interpreter = (memory interpreter)!!(cursor interpreter)

interpret' :: Interpreter -> Interpreter
interpret' interpreter@(Interpreter (list, Cursor c))
    | currentInstruction interpreter == 1  =  interpret' $ additionCommand interpreter
    | currentInstruction interpreter == 2  =  interpret' $ multiplicationCommand interpreter
    | otherwise = interpreter

(@@) :: Interpreter -> Int -> Int
(Interpreter (list, Cursor c)) @@ idx = list !! (c + idx)

(@!) :: Interpreter -> Int -> Int
(Interpreter (list, _)) @! idx = list !! idx

advance :: Interpreter -> Int -> Interpreter 
advance (Interpreter (list, Cursor c)) n = Interpreter (list, Cursor (c + n))

additionCommand :: Interpreter -> Interpreter
additionCommand interpreter = 
    advance (addition interpreter)
            4

addition :: Interpreter -> Interpreter
addition interpreter = put interpreter (interpreter @@ 3) ((interpreter @! (interpreter @@ 1)) + 
                   (interpreter @! (interpreter @@ 2)))

multiplication :: Interpreter -> Interpreter
multiplication interpreter = put interpreter (interpreter @@ 3) ((interpreter @! (interpreter @@ 1)) * 
                   (interpreter @! (interpreter @@ 2)))

multiplicationCommand :: Interpreter -> Interpreter
multiplicationCommand interpreter = 
    advance (multiplication interpreter) 
            4

update :: [Int] -> Int -> Int -> [Int] -- to delete
update list index value = take index list ++ [value] ++ drop (index + 1) list

put :: Interpreter -> Int -> Int -> Interpreter
put (Interpreter(list, c)) index value = Interpreter(take index list ++ [value] ++ drop (index + 1) list, c)

parse :: [Char] -> [Int]
parse input = convert $ (splitOn "," input)

convert :: [String] -> [Int]
convert = map read

spec :: Spec
spec = describe "Intcode Computer - Day 2" $ do
    -- 1,0,0,0,99 becomes 2,0,0,0,99 
    it "interprets 1 as addition" $ do
        interpret [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
        interpret [1,0,0,1,99] `shouldBe` [1,2,0,1,99]
        interpret [1,0,0,2,99] `shouldBe` [1,0,2,2,99]
    
    it "interprets two additions" $ do
        interpret [1,0,0,0,1,0,0,0,99] `shouldBe` [4,0,0,0,1,0,0,0,99]
    
    it "interprets 2 as multiplication" $ do
        interpret [2,3,0,3,99] `shouldBe` [2,3,0,6,99]

    it "interprets 2 multiplications" $ do
        interpret [2,3,0,3,2,3,0,3,99] `shouldBe` [2,3,0,12,2,3,0,3,99]

    it "can parse a from a string" $ do 
        parse "1,0,0,0,99" `shouldBe` [1,0,0,0,99]

    it "can reprogram our program" $ do
        let input =  parse "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,6,23,27,1,27,5,31,2,9,31,35,1,5,35,39,2,6,39,43,2,6,43,47,1,5,47,51,2,9,51,55,1,5,55,59,1,10,59,63,1,63,6,67,1,9,67,71,1,71,6,75,1,75,13,79,2,79,13,83,2,9,83,87,1,87,5,91,1,9,91,95,2,10,95,99,1,5,99,103,1,103,9,107,1,13,107,111,2,111,10,115,1,115,5,119,2,13,119,123,1,9,123,127,1,5,127,131,2,131,6,135,1,135,5,139,1,139,6,143,1,143,6,147,1,2,147,151,1,151,5,0,99,2,14,0,0"
        interpretReprogrammed input 12 0 `shouldBe` [337024,12,2,2,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,0,1,5,19,1,2,6,23,2,1,27,5,3,2,9,31,9,1,5,35,10,2,6,39,20,2,6,43,40,1,5,47,41,2,9,51,123,1,5,55,124,1,10,59,128,1,63,6,130,1,9,67,133,1,71,6,135,1,75,13,140,2,79,13,700,2,9,83,2100,1,87,5,2101,1,9,91,2104,2,10,95,8416,1,5,99,8417,1,103,9,8420,1,13,107,8425,2,111,10,33700,1,115,5,33701,2,13,119,168505,1,9,123,168508,1,5,127,168509,2,131,6,337018,1,135,5,337019,1,139,6,337021,1,143,6,337023,1,2,147,337023,1,151,5,0,99,2,14,0,0]


    it "99 ends the program" $ do 
        interpret  [99] `shouldBe` [99]


    describe "update" $ do
        it "update 0 to 1 on a single element list" $ do
          update [0] 0 1 `shouldBe` [1]


