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

-- World
type TurnCount = Int
data World = World {
  _entities       :: Entities,
  _turnCount      :: TurnCount,
  _pendingActions :: EntityActions,
  _nextEntityRef  :: EntityRef
} deriving (Show, Generic)
makeLenses ''World

type UpdatedWorld = World

instance Default World where
  def = World (IntMap.singleton 0 (Entity Player def True)) 0 IntMap.empty 1
