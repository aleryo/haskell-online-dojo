module AdventOfCodeIntcodeSpec where

import Test.Hspec

interpret :: [Int] -> [Int]
interpret list@(1 : p1 : p2 : p3 : _) =  update list p3  (list!!p1 + list!!p2)       
interpret list@(2 : p1 : p2 : p3 : _) =  update list p3  (list!!p1 * list!!p2)       
interpret xs = xs

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

    it "99 ends the program" $ do 
        interpret  [99] `shouldBe` [99]

    describe "update" $ do
        it "update 0 to 1 on a single element list" $ do
          update [0] 0 1 `shouldBe` [1]


update :: [Int] -> Int -> Int -> [Int]
update list index value = take index list ++ [value] ++ drop (index + 1) list