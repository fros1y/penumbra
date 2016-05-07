{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GameMonad where
import qualified Control.Monad.State as State
import Prelude hiding (Either (..), (.), id)
import Control.Category
import Control.Lens
import Data.IntMap.Strict as IntMap
import Data.Maybe (fromJust)
import Types
import Data.Default

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

newtype GameM a = GameM {
  runGame :: (State.StateT World IO) a
} deriving (Functor, Applicative, Monad, State.MonadState World, State.MonadIO)

doGame :: GameM a -> World -> IO (a, World)
doGame game = State.runStateT (runGame game)

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
