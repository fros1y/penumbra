{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Serialize where

import           Data.Aeson
import           Data.Map.Strict as Map
import           GHC.Generics

import           Actions
import           Coord
import           Direction
import           Entity
import           PlayerCommand
import           World

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
