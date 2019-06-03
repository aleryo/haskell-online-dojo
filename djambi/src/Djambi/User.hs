{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving         #-}

module Djambi.User where

import Data.Aeson
import GHC.Generics
import Djambi.Piece

data User = User { color :: Party }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)