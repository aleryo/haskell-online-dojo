module Main where

import Interpreter

-- Sample session
-- haskell-sqlite v0.1
-- >
-- > toto
-- unknown command: toto
-- > .exit
-- bye!
-- 
main :: IO ()
main = console
