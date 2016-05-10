{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GameMonad where
import           Control.Category
import           Control.Lens
import qualified Control.Monad.Random as Random
import qualified Control.Monad.State  as State
import           Data.Default
import           Data.IntMap.Strict   as IntMap
import           Data.Maybe           (fromJust, isNothing)
import           Prelude              hiding (Either (..), id, (.))
import qualified Data.Map.Lazy as Map

import           Entity
import           World
import Coord
import Obstruction

newtype GameM a = GameM {
  runGame :: (State.StateT World IO) a
} deriving (Functor, Applicative, Monad, State.MonadState World, State.MonadIO)

doGame :: GameM a -> World -> IO (a, World)
doGame game = State.runStateT (runGame game)

class (Monad m) => GameFunctions m where
  getWorld :: m World
  setWorld :: World -> m ()
  mkEntityRef :: m EntityRef
  addEntity :: Entity -> m EntityRef
  addEntity e = do
    ref <- mkEntityRef
    addEntityAt (ref, e)
    return ref
  addEntities :: [Entity] -> m [EntityRef]
  addEntities = mapM addEntity
  addEntityAt :: (EntityRef, Entity) -> m ()
  addEntitiesAt :: [(EntityRef, Entity)] -> m ()
  addEntitiesAt = mapM_ addEntityAt
  getPlayer :: m (EntityRef, Entity)
  setEntity :: EntityRef -> Entity -> m ()
  getEntity :: EntityRef -> m (Maybe Entity)

instance GameFunctions GameM where
  getWorld = State.get
  setWorld = State.put
  getPlayer = do
    ents <- use entities
    return (0, fromJust $ IntMap.lookup 0 ents)
  mkEntityRef = do
    ref <- use nextEntityRef
    nextEntityRef += 1
    return ref
  addEntityAt (eref, e) = entities %= insert where
    insert = IntMap.insert eref e
  setEntity r e = addEntityAt (r, e)
  getEntity r = do
    ents <- (use entities)
    return $ IntMap.lookup r ents

invalidateCache :: GameM ()
invalidateCache = do
  entityRefByCoord .= Map.empty

invalidateCacheFor :: Coord -> GameM ()
invalidateCacheFor coord = do
  cache <- use entityRefByCoord
  entityRefByCoord .= Map.delete coord cache

buildCacheFor :: Coord -> GameM ([EntityRef])
buildCacheFor coord = do
  cache <- use entityRefByCoord
  ents <- entitiesForCoord' coord
  entityRefByCoord .= Map.insert coord ents cache
  return ents

getCacheFor :: Coord -> GameM (Maybe [EntityRef])
getCacheFor coord = do
  cache <- use entityRefByCoord
  return $ Map.lookup coord cache

entitiesForCoord :: Coord -> GameM [EntityRef]
entitiesForCoord coord = do
  let helper (Just v) = return v
      helper Nothing = buildCacheFor coord
  cacheEntry <- getCacheFor coord
  helper cacheEntry

entitiesForCoord' :: Coord -> GameM [EntityRef]
entitiesForCoord' coord = do
  ents <- use entities
  let atCoord entity = (entity ^. entityPos) == coord
      ents' = IntMap.filter atCoord ents
  return $ Prelude.map fst (IntMap.toList ents')

checkCollision :: Coord -> GameM Bool
checkCollision coord = do
  let obstructsM = State.liftM obstructs
  entsAtCoord <- entitiesForCoord coord
  ents <- State.filterM (obstructsM . getEntity) entsAtCoord
  return $ not (Prelude.null ents)
