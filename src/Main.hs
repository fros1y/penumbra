{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Arrows #-}

import Prelude hiding (Either (..), (.), id)
import Control.Category
import Control.Lens
import Control.Applicative

import qualified Control.Monad.State as S
import qualified Control.Monad.Random as Random
import Control.Monad (unless)
import Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import Data.Default
import qualified Control.Auto as Auto

import Serialize
import Types
import UISFML
import Coord
import Entity

import Data.Maybe (fromJust, isNothing)

mkLevel :: Bounds -> TileMap -> EntityMap -> Types.Level
mkLevel b t e = Level {_tiles=t, _entities=e, _bounds=b}

mkBoringLevel :: Bounds -> Types.Level
mkBoringLevel bounds =
  let rock = mkWall $ Coord 1 1
  in mkLevel bounds (Map.fromList [rock]) Map.empty

combineTileMaps :: TileMap -> TileMap -> TileMap
combineTileMaps a b = Map.unionWith mappend a b

combineListTileMaps :: [TileMap] -> TileMap
combineListTileMaps ([m]) = m
combineListTileMaps (m:ms) = Prelude.foldr combineTileMaps m ms

mkRandomLevel :: Bounds -> IO Types.Level
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
  let ?context = context' in setup
  endDisplay context'

updatePlayer :: (Monad m) => Auto.Auto m (Maybe PlayerCommand) Coord
updatePlayer = proc input -> do
  let delta = case  input of
                    (Just (Go d))  -> fromDirection d
                    _       -> fromPair (0, 0)
  id -< delta

defLevel = mkRandomLevel (Bounds origin (Coord 40 40))

game :: (Monad m) => Auto.Auto m (Maybe PlayerCommand) World
game = proc input -> do
  turnCount' <- Auto.sumFrom 0 -< if isNothing input then 0 else 1
  delta <- updatePlayer -< input
  playerCoord' <- Auto.sumFrom_ (Coord 0 0) -< delta
  player' <- Auto.pure def -< ()
  id -< World {
                _turnCount = turnCount',
                _player = player',
                _currLevel = mkBoringLevel (Bounds origin (Coord 40 40)),
                _playerCoord = playerCoord'
              }


setup :: (?context :: DisplayContext) => IO ()
setup = do
  S.liftIO (Random.setStdGen $ Random.mkStdGen 1)
  gameLoop game Nothing

saveState :: World -> IO ()
saveState w = do
  let filename = "out.penumbra"
  writeFile filename $ show $ Aeson.encode w

loadState :: IO (Maybe World)
loadState = do
  let filename = "out.penumbra"
  fileContents <- readFile filename
  return $ Aeson.decode (read fileContents)

gameLoop :: (?context :: DisplayContext) => (Auto.Auto IO (Maybe PlayerCommand) World) -> (Maybe PlayerCommand) -> IO ()
gameLoop g input = do
  (worldState, g') <- Auto.stepAuto g input
  --S.liftIO $ print worldState
  render worldState
  input' <- S.liftIO getPlayerCommand
  gameLoop g' input'
