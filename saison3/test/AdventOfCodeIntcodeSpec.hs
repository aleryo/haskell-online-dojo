module AdventOfCodeIntcodeSpec where

import Test.Hspec

newtype Cursor = Cursor Int

interpret = fst . interpret' . fromList 

interpret' :: ([Int], Cursor) -> ([Int],Cursor)
interpret' (list, Cursor c )
    | list!!c == 1  =  interpret' ((addition list c), Cursor (c+4))
    | list!!c == 2  =  interpret' (multiplication list c, Cursor (c +4))
    | list!!c == 99  =  (list, Cursor c)

addition :: [Int] -> Int -> [Int]
addition tab cursor = update tab (tab!!(cursor+3)) (tab!!(tab!!(cursor+1)) + tab!!(tab!!(cursor+2)))

multiplication :: [Int] -> Int -> [Int]
multiplication tab cursor = update tab (tab!!(cursor+3)) (tab!!(tab!!(cursor+1)) * tab!!(tab!!(cursor+2)))


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

fromList l = (l, Cursor 0)

update :: [Int] -> Int -> Int -> [Int]
update list index value = take index list ++ [value] ++ drop (index + 1) list
