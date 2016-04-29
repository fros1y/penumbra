import Prelude hiding (Either (..), (.), id)

import Control.Category
import qualified Control.Monad.State as S
import Control.Monad.Random

import Types
import UI
import Coord

import Control.Monad (unless)
import Control.Lens

import Data.Map as Map

moveEntity :: Direction -> Entity -> Entity
moveEntity d = position %~ (+) (fromDirection d)

registerEntity :: Entity -> GameM EntityID
registerEntity entity = do
  newID <- use nextEntityID
  nextEntityID %= (+ 1)
  allEntities %= (Map.insert newID (entityID .~ newID $ entity))
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



mkRandomLevel :: Bounds -> GameM Types.Level
mkRandomLevel bounds = do
  let boundary = borderCoords bounds
  randomPillarLocations <- S.forM [1 .. 20] $ \_i -> randomWithin bounds
  rocks <- mapM mkBasicRock (randomPillarLocations ++ boundary)
  return $ mkLevel bounds rocks


main :: IO ()
main = do
  initDisplay
  let world = World {_nextEntityID = 1, _allEntities = Map.empty}
  S.runStateT setup world
  endDisplay

update :: Maybe PlayerCommand -> GameM ()
update command = updatePlayer command

updatePlayer :: Maybe PlayerCommand -> GameM ()
updatePlayer Nothing = return ()
updatePlayer (Just (Go d)) = do
  playerID <- use player
  playerEntity <- lookupEntitybyID_ playerID
  updateEntitybyID playerID $ moveEntity d playerEntity

setup :: GameM ()
setup = do
  S.liftIO (setStdGen $ mkStdGen 1)
  playerID <- mkPlayer (Coord 5 5)
  player .= playerID
  randomLevel <- mkRandomLevel $ Bounds origin (Coord 20 40)
  currLevel .= randomLevel
  gameLoop

gameLoop :: GameM ()
gameLoop = do
  render
  command <- S.liftIO getPlayerCommand
  Main.update command
  unless (command == Just Quit) gameLoop
