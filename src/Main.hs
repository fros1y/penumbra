import Prelude hiding (Either (..), (.), id)

import Control.Category
import qualified Control.Monad.State as S
import Control.Monad.Random
import Control.Monad (unless)
import Control.Lens
import Data.Map.Strict as Map

import Types
import UI
import Coord
import Entity

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
update Nothing = return ()
update (Just command) = updatePlayer command

updatePlayer :: PlayerCommand -> GameM ()
updatePlayer (Go d) = do
  playerID <- use player
  playerEntity <- lookupEntitybyID_ playerID
  updateEntitybyID playerID $ moveEntity d playerEntity
updatePlayer _ = return ()

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
