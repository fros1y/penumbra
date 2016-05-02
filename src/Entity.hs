module Entity where
import Prelude hiding (Either (..), (.), id)
import Control.Category
import qualified Control.Monad.State as S
import Types
import Control.Lens
import Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Default
import Coord

mkWall :: WorldCoord -> (WorldCoord, Maybe Tile)
mkWall coord = (coord, Just (Wall def))

mkPillar :: WorldCoord -> (WorldCoord, Maybe Tile)
mkPillar coord = (coord, Just (Pillar def))

mkTree :: WorldCoord -> (WorldCoord, Maybe Tile)
mkTree coord = (coord, Just (Tree def))
