{-# LANGUAGE DeriveGeneric   #-}

module Illuminate where
import           Control.Category
import           Control.Lens
import qualified Control.Monad.State as S
import           Data.Colour         as Colour
import           Data.Colour.Names   as Colour
import qualified Data.Map.Lazy       as LMap
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromJust)
import           Prelude             hiding (Either (..), id, (.))
import GHC.Generics

import           Coord


data LightSource = LightSource {
  _intensity :: Intensity,
  _color :: Colour.Colour Double
} deriving (Show, Eq, Generic)

type Intensity = Double

-- illuminateCoord :: Coord -> Illumination -> IlluminationMap -> IlluminationMap
-- illuminateCoord coord light illumMap = (at coord) %~ (fmap update) $ illumMap where
--   update Nothing = light
--   update (Just existing) = Colour.blend 2 light existing
