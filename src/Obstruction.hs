module Obstruction where
import           Control.Category
import           Control.Lens
import           Prelude          hiding (Either (..), id, (.))

import           Entity
import Coord


class Obstruction a where
  obstructs :: a -> Bool

instance Obstruction Entity where
  obstructs entity = obstructs (entity ^. entityType)

instance Obstruction EntityType where
  obstructs Floor = False
  obstructs _ = True

instance (Obstruction a) => Obstruction (Maybe a) where
  obstructs Nothing = False
  obstructs (Just a) = obstructs a
