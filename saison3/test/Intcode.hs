{-# LANGUAGE NamedFieldPuns #-}
module Intcode where

import Data.List.Split
import Data.Array
import Data.Function ((&))

newtype Cursor = Cursor { uncursor :: Int }
newtype Input = Input Int


data Interpreter = Interpreter { memory :: [Int]
                               , cursor :: Cursor
                               , input :: Input
                               , output :: Int
                               }

mkInterpreter :: [Int] -> Cursor -> Input -> Interpreter
mkInterpreter m c i = Interpreter m c i 42

newtype Setup = Setup (Int, Int)

data Result = Success Int | Failure

start :: [Int] -> Interpreter
start startingMemory = mkInterpreter startingMemory (Cursor 0) (Input 0)

startWithInput :: [Int] -> Int -> Interpreter
startWithInput startingMemory input = mkInterpreter startingMemory (Cursor 0) (Input input)

interpret :: [Int] -> [Int]
interpret = memory . interpret' . start

interpretWithInput :: [Int] -> Int -> Interpreter
interpretWithInput startingMemory input = interpret' $ startWithInput startingMemory input

interpretReprogrammed :: [Int] -> Int -> Int -> [Int]
interpretReprogrammed interpreter a b  = memory  $ interpret'  $ (put (put (start interpreter) 1 a ) 2 b)

currentInstruction :: Interpreter -> Int
currentInstruction interpreter = (memory interpreter)!!(uncursor $ cursor interpreter)

interpret' :: Interpreter -> Interpreter
interpret' interpreter@(Interpreter {cursor = Cursor c})
    | currentInstruction interpreter == 1  =  interpret' $ additionCommand interpreter
    | currentInstruction interpreter == 2  =  interpret' $ multiplicationCommand interpreter
    | currentInstruction interpreter == 3  =  interpret' $ inputCommand interpreter
    | currentInstruction interpreter == 4  =  interpret' $ outputCommand interpreter
    | otherwise = interpreter

(@@) :: Interpreter -> Int -> Int
(Interpreter {memory = list, cursor =  Cursor c}) @@ idx = list !! (c + idx)

(@!) :: Interpreter -> Int -> Int
(Interpreter {memory = list}) @! idx = list !! idx

(@!!) :: Interpreter -> Int -> Int
interp @!! idx = interp @! (interp @@ idx)  

advance :: Interpreter -> Int -> Interpreter 
advance (interp@Interpreter {cursor = Cursor c}) n = interp { cursor = Cursor $ c + n }

additionCommand :: Interpreter -> Interpreter
additionCommand interpreter = 
    advance (addition interpreter)
            4

inputCommand :: Interpreter -> Interpreter
inputCommand interpreter = 
    advance (inputFn interpreter)
            2

outputCommand :: Interpreter -> Interpreter
outputCommand interpreter = 
    advance (outputFn interpreter)
            2

inputFn :: Interpreter -> Interpreter
inputFn interpreter@(Interpreter {memory = list, input = Input i}) = put interpreter (interpreter @@ 1) i

outputFn :: Interpreter -> Interpreter
outputFn interp = interp {output = interp @!! 1}


addition :: Interpreter -> Interpreter
addition interpreter = put interpreter (interpreter @@ 3) 
                    ((interpreter @!! 1) + (interpreter @!! 2))

multiplication :: Interpreter -> Interpreter
multiplication interpreter = put interpreter (interpreter @@ 3) 
   ((interpreter @!! 1) * (interpreter @!!  2))

multiplicationCommand :: Interpreter -> Interpreter
multiplicationCommand interpreter = 
    advance (multiplication interpreter) 
            4

update :: [Int] -> Int -> Int -> [Int] -- to delete
update list index value = take index list ++ [value] ++ drop (index + 1) list

put :: Interpreter -> Int -> Int -> Interpreter
put (interp@Interpreter{memory = list}) index value = interp { memory = (take index list ++ [value] ++ drop (index + 1) list) }

parse :: [Char] -> [Int]
parse input = convert $ (splitOn "," input)

convert :: [String] -> [Int]
convert = map read

findParameters :: [Int] -> Int -> ( Int, Int)
findParameters input result =
    [(head (interpretReprogrammed input a b), (a, b)) | a <- [0..99], b <- [0..99]]
    & filter (\(actualResult, _) -> actualResult == result)
    & head
    & snd    

decodeInstruction :: Int -> [Int]
decodeInstruction _ = [0,1,0,2]