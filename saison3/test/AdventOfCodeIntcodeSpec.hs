module AdventOfCodeIntcodeSpec where

import Test.Hspec
import Intcode


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
        interpretReprogrammed input 12 2 `shouldBe` [4484226,12,2,2,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,24,1,5,19,25,2,6,23,50,1,27,5,51,2,9,31,153,1,5,35,154,2,6,39,308,2,6,43,616,1,5,47,617,2,9,51,1851,1,5,55,1852,1,10,59,1856,1,63,6,1858,1,9,67,1861,1,71,6,1863,1,75,13,1868,2,79,13,9340,2,9,83,28020,1,87,5,28021,1,9,91,28024,2,10,95,112096,1,5,99,112097,1,103,9,112100,1,13,107,112105,2,111,10,448420,1,115,5,448421,2,13,119,2242105,1,9,123,2242108,1,5,127,2242109,2,131,6,4484218,1,135,5,4484219,1,139,6,4484221,1,143,6,4484223,1,2,147,4484225,1,151,5,0,99,2,14,0,0]
   
    it "can find input parameters for result 19690720" $ do
        let input =  parse "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,6,23,27,1,27,5,31,2,9,31,35,1,5,35,39,2,6,39,43,2,6,43,47,1,5,47,51,2,9,51,55,1,5,55,59,1,10,59,63,1,63,6,67,1,9,67,71,1,71,6,75,1,75,13,79,2,79,13,83,2,9,83,87,1,87,5,91,1,9,91,95,2,10,95,99,1,5,99,103,1,103,9,107,1,13,107,111,2,111,10,115,1,115,5,119,2,13,119,123,1,9,123,127,1,5,127,131,2,131,6,135,1,135,5,139,1,139,6,143,1,143,6,147,1,2,147,151,1,151,5,0,99,2,14,0,0"
        findParameters input 19690720 `shouldBe` (56, 96) 
    
    it "can use input data" $ do
        let input = parse "3,0,99"
        memory  (interpretWithInput input 42 ) `shouldBe` [42,0,99] 
    
    it "can output data" $ do
        let input = parse "3,0,4,0,99"
        output (interpretWithInput input 42) `shouldBe` 42
        output (interpretWithInput input 41) `shouldBe` 41    
    
    xit "can handle different parameter modes" $ do
        let input = parse "01002,4,3,4,33"
        memory  (interpretWithInput input 0) `shouldBe` [1002,4,3,4,99] 

    it "99 ends the program" $ do 
        interpret  [99] `shouldBe` [99]

    describe "Memory Reader" $ do

        it "it decodes instruction format" $ do
            decodeInstruction 1002 `shouldBe` Instruction [0,1,0,2]
            decodeInstruction 102 `shouldBe` Instruction [0,0,1,2]
            decodeInstruction 10002 `shouldBe` Instruction [1,0,0,2]
            decodeInstruction 10001 `shouldBe` Instruction [1,0,0,1]

        it "should gives me hundreds" $ do 
            hundreds 12345 `shouldBe` 3

    describe "update" $ do
        it "update 0 to 1 on a single element list" $ do
          update [0] 0 1 `shouldBe` [1]


