{-# LANGUAGE ImplicitParams #-}
module Level where

import           Control.Category
import           Control.Lens
import           Control.Monad        (unless)
import qualified Control.Monad.Random as Random
import qualified Control.Monad.State  as S
import qualified Data.Aeson           as Aeson
import           Data.Default
import           Data.Map.Strict      as Map
import           Prelude              hiding (Either (..), id, (.))

import           Coord
import           Entity
import           GameMonad
import           Serialize
import           UISFML


(<>) :: Monoid m => m -> m -> m
(<>) = mappend

instance Monoid EntityType where
  mempty = Floor
  mappend x Floor = x
  mappend Floor x = x
  mappend x y = x

newtype LevelBuilder = LevelBuilder { asMap :: CoordMap EntityType } deriving Show

instance Monoid LevelBuilder where
  mempty = LevelBuilder Map.empty
  mappend x y = LevelBuilder $ Map.unionWith (<>) (asMap x) (asMap y)
  mconcat list = LevelBuilder $ Map.unionsWith (<>) (Prelude.map asMap list)

setTile :: (Coord, EntityType) -> LevelBuilder -> LevelBuilder
setTile (coord, tileType) (LevelBuilder builder) = LevelBuilder $ Map.insert coord tileType builder

getTile :: Coord -> LevelBuilder -> Maybe EntityType
getTile coord (LevelBuilder builder) = Map.lookup coord builder

mkTiles :: EntityType -> [Coord] -> LevelBuilder
mkTiles tileType = Prelude.foldr set mempty where
  set coord = setTile (coord, tileType)

fromLevelBuilder :: LevelBuilder -> [Entity]
fromLevelBuilder (LevelBuilder builder) = Prelude.map fromEntityType (toList builder)

fromEntityType :: (Coord, EntityType) -> Entity
fromEntityType (coord, tileType) = Entity {_entityType=tileType, _entityPos=coord, _entityAlive = True}

mkFloors :: Bounds -> LevelBuilder
mkFloors bounds = mkTiles Floor $ coordsWithin bounds

mkBounds :: Bounds -> LevelBuilder
mkBounds bounds = mkTiles Wall $ borderCoords bounds

conflict :: LevelBuilder -> LevelBuilder -> Bool
conflict (LevelBuilder builder1) (LevelBuilder builder2) = (Map.size intersect) /= 0 where
  intersect = Map.intersection builder1 builder2

mkBoringLevel :: Bounds -> [Entity]
mkBoringLevel bounds = fromLevelBuilder $ boundary <> flooring where
  boundary = mkBounds bounds
  flooring = mkFloors bounds

mkRandomLevel :: Bounds -> GameM [Entity]
mkRandomLevel bounds = do
  randomPillarLocations <- S.liftIO $ S.forM [1 .. 50] $ \_i -> randomWithin (insetBounds 4 bounds)
  let boundary = mkBounds bounds
      flooring = mkFloors bounds
      pillars = mkTiles Wall randomPillarLocations
  return $ fromLevelBuilder $ boundary <> pillars <> flooring
