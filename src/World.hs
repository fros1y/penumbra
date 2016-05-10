{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module World where
import           Control.Category
import           Control.Lens
import           Data.Default
import           Data.IntMap.Strict as IntMap
import           GHC.Generics
import           Prelude            hiding (Either (..), id, (.))

import           Actions
import           Entity
import Coord

-- World
type TurnCount = Int
type EntityRefByCoord = CoordMap [EntityRef]

data World = World {
  _entities       :: Entities,
  _turnCount      :: TurnCount,
  _pendingActions :: EntityActions,
  _nextEntityRef  :: EntityRef,
  _entityRefByCoord :: EntityRefByCoord
} deriving (Show, Generic)
makeLenses ''World

type UpdatedWorld = World

instance Default World where
  def = World (IntMap.singleton 0 (Entity Player def True Nothing)) 0 IntMap.empty 1 def
