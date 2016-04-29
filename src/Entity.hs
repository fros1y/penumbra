module Entity where
import Prelude hiding (Either (..), (.), id)
import Control.Category
import qualified Control.Monad.State as S
import Types
import Control.Lens
import Data.Map.Strict as Map
import Data.Maybe (fromJust)

import Coord

lookupEntitybyID :: EntityID -> GameM (Maybe Entity)
lookupEntitybyID key = use (entityFor key)

lookupEntitybyID_ :: EntityID -> GameM (Entity)
lookupEntitybyID_ key = do
  e <- lookupEntitybyID key
  return $ fromJust e

updateEntitybyID :: EntityID -> Entity -> GameM ()
updateEntitybyID eID e = entityFor eID ?= e

entityFor eID = allEntities . at eID

moveEntity :: Direction -> Entity -> Entity
moveEntity d = position %~ (+) (fromDirection d)

registerEntity :: Entity -> GameM EntityID
registerEntity entity = do
  newID <- nextEntityID <+= 1
  entityFor newID ?= (entityID .~ newID $ entity)
  return newID

normalObstruction :: EntityBuilder
normalObstruction = obstruction .~ Just obs where
  obs = Obstruction {_transparent = False, _flyOver = False}

obstructingTile :: EntityBuilder
obstructingTile = normalObstruction .
                  (entityType .~ Tile) .
                  (damageable .~ Nothing) .
                  (behavior .~ Nothing)

baseEntity :: Coord -> Symbol -> Entity
baseEntity pos sym =  (position .~ pos) .
                      (symbol .~ sym) .
                      (behavior .~ Nothing) $ Entity {_entityID = -1}

mkBasicRock :: Coord -> GameM EntityID
mkBasicRock coord = registerEntity $ obstructingTile $ baseEntity coord '#'

mkPlayer :: Coord -> GameM EntityID
mkPlayer coord = registerEntity $ baseEntity coord '@'

mkLevel :: Bounds -> [EntityID] -> Types.Level
mkLevel boundary entities = (entityIDs .~ entities) .
                 (bounds .~ boundary) $ Level {}

addEntityID :: EntityID -> Types.Level -> Types.Level
addEntityID eID level = entityIDs %~ addE $ level where
  addE list = eID : list
