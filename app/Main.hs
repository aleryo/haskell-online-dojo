module Main where

import Interpreter
import System.IO

-- Sample session
-- haskell-sqlite v0.1
-- >
-- > toto
-- unknown command: toto
-- > .exit
-- bye!
--
main :: IO ()
main = hSetBuffering stdout NoBuffering >> hSetBuffering stderr NoBuffering >> console
