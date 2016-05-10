{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Serialize where

import           Data.Aeson
import           Data.Map.Strict as Map
import           GHC.Generics
import           Data.Colour         as Colour
import           Data.Colour.Names   as Colour

import           Actions
import           Coord
import           Direction
import           Entity
import           PlayerCommand
import           World
import Illuminate

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

instance ToJSON LightSource
instance FromJSON LightSource

instance ToJSON Entity
instance FromJSON Entity

instance ToJSON Action
instance FromJSON Action

instance ToJSON EntityType
instance FromJSON EntityType

instance ToJSON (Colour.Colour Double) where
  toJSON = toJSON . show

instance FromJSON (Colour.Colour Double) where
  parseJSON  = fmap read . parseJSON

instance (ToJSON a, Show a) => ToJSON (CoordMap a) where
  toJSON = toJSON . show

instance (FromJSON a, Read a) => FromJSON (CoordMap a) where
  parseJSON = fmap read. parseJSON
