import Prelude hiding (Either (..), (.), id)
import Control.Category
import Control.Lens

import qualified Control.Monad.State as S
import Control.Monad.Random
import Control.Monad (unless)
import Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import Data.Default

import Serialize
import Types
import UISFML
import Coord
import Entity

import Data.Maybe (fromJust)

mkLevel :: Bounds -> TileMap -> EntityMap -> Types.Level
mkLevel b t e = Level {_tiles=t, _entities=e, _bounds=b}

mkRandomLevel :: Bounds -> GameM Types.Level
mkRandomLevel bounds = do
  let boundary = borderCoords bounds
  randomPillarLocations <- S.forM [1 .. 20] $ \_i -> randomWithin bounds
  let rocks = fmap mkWall (randomPillarLocations ++ boundary)
  return $ mkLevel bounds (Map.fromList rocks) Map.empty

main :: IO ()
main = do
  context <- initDisplay
  S.runStateT (setup context) def
  endDisplay context

update :: Maybe PlayerCommand -> GameM ()
update Nothing = return ()
update (Just command) = updatePlayer command

updatePlayer :: PlayerCommand -> GameM ()
updatePlayer (Go d) = do
  playerCoord += fromDirection d
updatePlayer _ = return ()

setup :: DisplayContext -> GameM ()
setup context = do
  S.liftIO (setStdGen $ mkStdGen 1)
  playerCoord .= Coord 5 5
  randomLevel <- mkRandomLevel $ Bounds origin (Coord 20 40)
  currLevel .= randomLevel
  turnCount .= 0
  gameLoop context

saveState :: World -> IO ()
saveState w = do
  let filename = "out.penumbra"
  writeFile filename $ show $ Aeson.encode w

loadState :: IO (Maybe World)
loadState = do
  let filename = "out.penumbra"
  fileContents <- readFile filename
  return $ Aeson.decode (read fileContents)

gameLoop :: DisplayContext -> GameM ()
gameLoop context = do
  render context
  command <- S.liftIO $ getPlayerCommand context
  case command of
    Just Quit -> return ()
    Just Save -> do
      state <- S.get
      S.liftIO $ saveState state
      gameLoop context
    Just Load -> do
      state <- S.liftIO loadState
      S.put (fromJust state)
      gameLoop context
    Nothing -> gameLoop context
    _ -> do Main.update command; gameLoop context
