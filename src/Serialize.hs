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
instance ToJSON Types.Level
instance FromJSON Types.Level
instance ToJSON World
instance FromJSON World
instance ToJSON PlayerCommand
instance FromJSON PlayerCommand
instance ToJSON Specifics
instance FromJSON Specifics
instance ToJSON Tile
instance FromJSON Tile
instance ToJSON Entity
instance FromJSON Entity

-- FIXME This seems like an evil hack, but can't get Coord-based keys to work
-- with ToJSON. Strings, however, work fine.

instance (ToJSON v, Show v) => ToJSON (Map Coord v) where
  toJSON = toJSON . Map.mapKeys show

instance (FromJSON v, Read v) => FromJSON (Map Coord v) where
  parseJSON = fmap (Map.mapKeys read) . parseJSON
