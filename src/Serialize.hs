{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Serialize where
import Data.Aeson
import GHC.Generics
import Data.Map.Strict as Map

import Coord
import Types

instance ToJSON Direction
instance FromJSON Direction

instance ToJSON Coord
instance FromJSON Coord

instance ToJSON Bounds
instance FromJSON Bounds

instance ToJSON World
instance FromJSON World

instance ToJSON PlayerCommand
instance FromJSON PlayerCommand

instance ToJSON Entity
instance FromJSON Entity

instance ToJSON Action
instance FromJSON Action

instance ToJSON EntityType
instance FromJSON EntityType
