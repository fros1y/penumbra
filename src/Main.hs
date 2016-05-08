{-# LANGUAGE ImplicitParams #-}

import           Control.Category
import           Control.Lens
import           Prelude              hiding (Either (..), id, (.))

import           Control.Monad        (foldM, unless)
import qualified Control.Monad.Random as Random
import qualified Control.Monad.State  as S
import qualified Data.Aeson           as Aeson
import           Data.Default
import           Data.IntMap.Strict   as IntMap
import           Data.Map.Strict      as Map
import           Debug.Trace

import           Coord
import           GameMonad
import           Types
import           UISFML
-- import Entity
import           Level
import           Serialize
import           State

import           Data.Maybe           (fromJust)

getUserAction :: (?context :: DisplayContext) => PlayerCommand -> Actions
getUserAction (Go direction) = [ActMoveBy (fromDirection direction)]
getUserAction _ = []

main :: IO ()
main = do
  context' <- initDisplay
  let ?context = context' in (doGame setup def)
  endDisplay context'

setup :: (?context :: DisplayContext) => GameM ()
setup = do
  let player = Entity {_entityType=Player, _entityPos=Coord 20 20, _entityAlive=True}
  addEntityAt (0, player)
  level <- mkRandomLevel (Bounds origin (Coord 40 40))
  addEntities level
  gameLoop
  return ()

gameLoop :: (?context :: DisplayContext) => GameM ()
gameLoop = do
  render
  command <- S.liftIO getPlayerCommand
  case command of
    Nothing -> gameLoop
    Just Quit -> return ()
    Just Save -> do
      saveWorld <- getWorld
      S.liftIO $ saveState saveWorld
      gameLoop
    Just Load -> do
      loadWorld <- S.liftIO $ loadState
      setWorld (fromJust loadWorld)
      gameLoop
    Just userCommand -> do
      let userAction = getUserAction userCommand
      updateWorld userAction
      gameLoop

updateWorld :: Actions -> GameM ()
updateWorld actions = do
  ents <- use entities
  let moveOrder = id (IntMap.toList ents)
  mapM_ (updateEntity actions) moveOrder

updateEntity :: Actions -> (EntityRef, Entity) -> GameM ()
updateEntity actions e@(0, _) = updatePlayer actions e
updateEntity _ _ = return ()

entitiesAtCoord :: Coord -> GameM [Entity]
entitiesAtCoord coord = do
  ents <- use entities
  let atCoord entity = (entity ^. entityPos) == coord
      ents' = IntMap.filter atCoord ents
  return $ Prelude.map snd (IntMap.toList ents')

checkCollision :: Coord -> GameM Bool
checkCollision coord = do
  entsAtCoord <- entitiesAtCoord coord
  let ents = Prelude.filter obstructs entsAtCoord
      foo = traceShow (length ents) ()
  return $ (length ents) > 0

moveCheckingForCollision :: DeltaCoord -> (EntityRef, Entity) -> GameM Bool
moveCheckingForCollision delta (ref, ent) = do
  let target = (ent ^. entityPos) + delta
  collision <- checkCollision target
  S.unless collision $ setEntity ref (ent & entityPos .~ target)
  return (not collision)

updatePlayer :: Actions -> (EntityRef, Entity) -> GameM ()
updatePlayer (ActMoveBy delta:as) e = do
  moveCheckingForCollision delta e
  updatePlayer as e
updatePlayer [] e = return ()
