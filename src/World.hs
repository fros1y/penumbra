{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module World where
import           Control.Category
import           Control.Lens
import           Data.Default
import           Data.IntMap.Strict as IntMap
import Data.Map.Lazy as Map
import           GHC.Generics
import           Prelude            hiding (Either (..), id, (.))

import           Actions
import           Entity
import Coord
import Memory

-- World
type TurnCount = Int
type EntityRefByCoord = CoordMap [EntityRef]

data World = World {
  _entities       :: Entities,
  _turnCount      :: TurnCount,
  _pendingActions :: EntityActions,
  _nextEntityRef  :: EntityRef,
  _entityRefByCoord :: EntityRefByCoord,
  _playerMemory  :: Memory
} deriving (Show, Generic)
makeLenses ''World

type UpdatedWorld = World

instance Default World where
  def = World (IntMap.singleton 0 (Entity 0 Player def True Nothing)) 0 def 1 def def
