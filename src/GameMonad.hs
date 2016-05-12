{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GameMonad where
import           Control.Category
import           Control.Lens
import qualified Control.Monad.Random as Random
import qualified Control.Monad.State  as State
import           Data.Default
import           Data.IntMap.Strict   as IntMap
import           Data.Maybe           (fromJust, isNothing, isJust)
import           Prelude              hiding (Either (..), id, (.))
import qualified Data.Map.Lazy as Map

import           Entity
import           World
import Coord
import Obstruction
import Memory

newtype GameM a = GameM {
  runGame :: (State.StateT World IO) a
} deriving (Functor, Applicative, Monad, State.MonadState World, State.MonadIO)

doGame :: GameM a -> World -> IO (a, World)
doGame game = State.runStateT (runGame game)

addEntity :: Entity -> GameM EntityRef
addEntity e = do
  ref <- mkEntityRef
  addEntityAt (ref, e)
  return ref

addEntities :: [Entity] -> GameM [EntityRef]
addEntities = mapM addEntity

addEntitiesAt :: [(EntityRef, Entity)] -> GameM ()
addEntitiesAt = mapM_ addEntityAt

getWorld :: GameM World
getWorld = State.get

setWorld :: World -> GameM ()
setWorld = State.put

mkEntityRef :: GameM EntityRef
mkEntityRef = do
  ref <- use nextEntityRef
  nextEntityRef += 1
  return ref

addEntityAt :: (EntityRef, Entity) -> GameM ()
addEntityAt (eref, e) = do
  let e' = (set entityRef eref) e
  entities %= (IntMap.insert eref e')
  invalidateCacheBecauseOf e'

setEntity :: Entity -> GameM ()
setEntity e = addEntityAt ((e ^. entityRef), e)

getPlayer :: GameM Entity
getPlayer = getEntityPartial 0

getEntity :: EntityRef -> GameM (Maybe Entity)
getEntity r = do
  ents <- (use entities)
  return $ IntMap.lookup r ents

getEntityPartial :: EntityRef -> GameM (Entity)
getEntityPartial r = do
  ents <- (use entities)
  return $ fromJust (IntMap.lookup r ents)

invalidateCache :: GameM ()
invalidateCache = entityRefByCoord .= Map.empty

invalidateCacheFor :: Coord -> GameM ()
invalidateCacheFor coord = do
  cache <- use entityRefByCoord
  entityRefByCoord .= Map.delete coord cache

invalidateCacheBecauseOf :: Entity -> GameM ()
invalidateCacheBecauseOf e = invalidateCacheFor (e ^. entityPos)

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

moveEntity :: EntityRef -> Coord -> GameM ()
moveEntity ref coord = do
  e <- getEntity ref
  State.when (isJust e) $ do
    let e' = fromJust e
    invalidateCacheBecauseOf e'
    setEntity (e' & entityPos .~ coord)

moveCheckingForCollision :: DeltaCoord -> EntityRef -> GameM Bool
moveCheckingForCollision delta ref = do
  ent <- getEntityPartial ref
  let target = (ent ^. entityPos) + delta
  collision <- checkCollision target
  State.unless collision $ moveEntity ref target
  return (not collision)

inMemory :: Entity -> GameM (Bool)
inMemory e = do
  locations <- use (playerMemory . entityLocations)
  let memory = IntMap.lookup (e ^. entityRef) locations
  return $ case memory of
    Nothing -> False
    (Just coord) -> coord == (e ^. entityPos)

remember :: Entity -> GameM ()
remember e = do (playerMemory . entityLocations) %= update where
  ref = (e ^. entityRef)
  pos = (e ^. entityPos)
  update memory = IntMap.insert ref pos (IntMap.delete (e ^. entityRef) memory)
