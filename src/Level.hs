{-# LANGUAGE ImplicitParams #-}
module Level where

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
