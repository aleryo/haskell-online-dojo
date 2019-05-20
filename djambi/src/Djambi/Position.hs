{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving         #-}
module Djambi.Position where

import           Control.Monad
import Data.Aeson
import GHC.Generics

data Index = A | B | C | D | E | F | G | H | I
  deriving (Enum, Bounded, Eq, Ord, Show, Generic)

instance ToJSON Index
instance FromJSON Index

newtype Col = Col { col :: Index }
  deriving newtype (Enum, Bounded, Eq, Ord, Num, ToJSON, FromJSON)

type Row = Index

instance Show Col where
    show c = case col c of
        A -> "1"
        B -> "2"
        C -> "3"
        D -> "4"
        E -> "5"
        F -> "6"
        G -> "7"
        H -> "8"
        I -> "9"

instance Num Index where
    fromInteger 1 = A
    fromInteger 2 = B
    fromInteger 3 = C
    fromInteger 4 = D
    fromInteger 5 = E
    fromInteger 6 = F
    fromInteger 7 = G
    fromInteger 8 = H
    fromInteger 9 = I
    fromInteger _ = error "out of bounds"

    (+)    _ _ = error "Unsupported"
    (*)    _ _ = error "Unsupported"
    (-)    _ _ = error "Unsupported"
    signum _   = error "Unsupported"
    abs    _   = error "Unsupported"

type Position = (Row, Col)

safeShift :: (Bounded a, Ord a, Enum a) => a -> Integer -> Maybe a
safeShift value shift
  | shift == 0 = pure value
  | shift >  0 = guard (value /= maxBound) *> safeShift (succ value) (pred shift)
  | otherwise  = guard (value /= minBound) *> safeShift (pred value) (succ shift)
