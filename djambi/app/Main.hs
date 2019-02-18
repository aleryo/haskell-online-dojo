module Main where

import           Djambi.Server
import           Network.Wai.Handler.Warp

main :: IO ()
main = djambiApp >>= run 3333
