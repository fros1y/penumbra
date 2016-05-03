{-# LANGUAGE ImplicitParams #-}

import Prelude hiding (Either (..), (.), id)
import Control.Category
import Control.Lens

import qualified Control.Monad.State as S
import qualified Control.Monad.Random as Random
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

mkBoringLevel :: Bounds -> GameM Types.Level
mkBoringLevel bounds = do
  let rock = mkWall $ Coord 1 1
  return $ mkLevel bounds (Map.fromList [rock]) Map.empty

combineTileMaps :: TileMap -> TileMap -> TileMap
combineTileMaps a b = Map.unionWith mappend a b

combineListTileMaps :: [TileMap] -> TileMap
combineListTileMaps ([m]) = m
combineListTileMaps (m:ms) = Prelude.foldr combineTileMaps m ms

mkRandomLevel :: Bounds -> GameM Types.Level
mkRandomLevel bounds = do
  let boundary = borderCoords bounds
  randomPillarLocations <- S.forM [1 .. 10] $ \_i -> randomWithin (insetBounds 2 bounds)
  randomTreeLocations <- S.forM [1..10] $ \_i -> randomWithin (insetBounds 2 bounds)
  let empty   =   Map.fromList $ mkEmpty <$> coordsWithin bounds
      rocks   =   Map.fromList $ mkWall <$> boundary
      trees   =   Map.fromList $ mkTree <$> randomTreeLocations
      pillars =   Map.fromList $ mkPillar <$> randomPillarLocations
      floors  =   Map.fromList $ mkFloor <$> coordsWithin bounds
      combined =  combineListTileMaps [empty, rocks, trees, pillars, floors]
  return $ mkLevel bounds combined Map.empty

main :: IO ()
main = do
  context' <- initDisplay
  let ?context = context' in S.runStateT setup def
  endDisplay context'

update :: Maybe PlayerCommand -> GameM ()
update Nothing = return ()
update (Just command) = updatePlayer command

updatePlayer :: PlayerCommand -> GameM ()
updatePlayer (Go d) = do
  playerCoord += fromDirection d
updatePlayer _ = return ()

setup :: (?context :: DisplayContext) => GameM ()
setup = do
  S.liftIO (Random.setStdGen $ Random.mkStdGen 1)
  playerCoord .= Coord 10 10
  clevel <- mkRandomLevel $ Bounds origin (Coord 100 100)
  currLevel .= clevel
  turnCount .= 0
  gameLoop

saveState :: World -> IO ()
saveState w = do
  let filename = "out.penumbra"
  writeFile filename $ show $ Aeson.encode w

loadState :: IO (Maybe World)
loadState = do
  let filename = "out.penumbra"
  fileContents <- readFile filename
  return $ Aeson.decode (read fileContents)

gameLoop :: (?context :: DisplayContext) => GameM ()
gameLoop = do
  render
  command <- S.liftIO getPlayerCommand
  case command of
    Just Quit -> return ()
    Just Save -> do
      state <- S.get
      S.liftIO $ saveState state
      gameLoop
    Just Load -> do
      state <- S.liftIO loadState
      S.put (fromJust state)
      gameLoop
    Nothing -> gameLoop
    _ -> do Main.update command; gameLoop
