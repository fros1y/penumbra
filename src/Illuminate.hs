-- type IlluminationMap = LMap.Map Coord (Maybe Illumination)

module Illuminate where
import Data.Colour as Colour
import Data.Colour.Names as Colour
import Prelude hiding (Either (..), (.), id)
import Control.Lens
import Control.Category
import qualified Control.Monad.State as S
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as LMap

import Data.Maybe (fromJust)
import Types
import Coord


type Intensity = Double

-- illuminateCoord :: Coord -> Illumination -> IlluminationMap -> IlluminationMap
-- illuminateCoord coord light illumMap = (at coord) %~ (fmap update) $ illumMap where
--   update Nothing = light
--   update (Just existing) = Colour.blend 2 light existing
