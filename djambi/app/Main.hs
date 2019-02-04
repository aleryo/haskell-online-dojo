module Main where

import           Djambi.Server
import           Network.Wai.Handler.Warp

main :: IO ()
main = run 3333 djambiServer
