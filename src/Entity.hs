{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Entity where
import           Control.Category
import           Control.Lens
import           Data.Default
import           Data.IntMap.Strict as IntMap
import           Data.Maybe         (fromJust)
import           GHC.Generics
import           Prelude            hiding (Either (..), id, (.))

import           Coord

-- Entity
type EntityRef = Int
type TargetEntityRef = EntityRef

data EntityType = Player |
                  Rat |
                  Frog |
                  Wall |
                  Floor
                  deriving (Show, Eq, Generic)

data Entity = Entity {
  _entityType  :: EntityType,
  _entityPos   :: Coord,
  _entityAlive :: Bool
} deriving (Show, Generic)
makeLenses ''Entity

type UpdatedEntity = Entity
type EntityRefMap a = IntMap.IntMap a
type Entities = EntityRefMap Entity
