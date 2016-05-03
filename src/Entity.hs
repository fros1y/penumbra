module Entity where
import Prelude hiding (Either (..), (.), id)
import Control.Category

import Data.Maybe (fromJust)
import Coord
import Types
import Data.Default

mkWall :: WorldCoord -> (WorldCoord, Maybe Tile)
mkWall coord = (coord, Just (Wall def))

mkPillar :: WorldCoord -> (WorldCoord, Maybe Tile)
mkPillar coord = (coord, Just (Pillar def))

mkTree :: WorldCoord -> (WorldCoord, Maybe Tile)
mkTree coord = (coord, Just (Tree def))

mkFloor :: WorldCoord -> (WorldCoord, Maybe Tile)
mkFloor coord = (coord, Just (Floor def))

mkEmpty :: WorldCoord -> (WorldCoord, Maybe Tile)
mkEmpty coord = (coord, Just (EmptyTile))
