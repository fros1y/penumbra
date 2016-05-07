{-# LANGUAGE ImplicitParams #-}

import Prelude hiding (Either (..), (.), id)
import Control.Category
import Control.Lens

import qualified Control.Monad.State as S
import qualified Control.Monad.Random as Random
import Control.Monad (unless, foldM)
import Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import Data.Default
import Data.IntMap.Strict as IntMap

-- import Serialize
import Types
import UISFML
import Coord
import GameMonad
-- import Entity
-- import Level
-- import State

import Data.Maybe (fromJust)

getUserAction :: (?context :: DisplayContext) => PlayerCommand -> Actions
getUserAction (Go direction) = [ActMoveBy (fromDirection direction)]
getUserAction _ = []

main :: IO ()
main = do
  context' <- initDisplay
  let ?context = context' in (doGame setup def)
  endDisplay context'

mkWall :: Coord -> Entity
mkWall coord = Entity {_entityType=Wall, _entityPos=coord, _entityAlive = True}

setup :: (?context :: DisplayContext) => GameM ()
setup = do
  let player = Entity {_entityType=Player, _entityPos=Coord 0 0, _entityAlive=True}
  addEntityAt (0, player)
  addEntities [mkWall (Coord 4 4), mkWall (Coord 2 5)]
  gameLoop
  return ()

gameLoop :: (?context :: DisplayContext) => GameM ()
gameLoop = do
  render
  command <- S.liftIO getPlayerCommand
  case command of
    Nothing -> gameLoop
    Just Quit -> return ()
    -- Just Save -> do
    --   gameLoop w' actions
    -- Just Load -> do
    --   gameLoop w' actions
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

updatePlayer :: Actions -> (EntityRef, Entity) -> GameM ()
updatePlayer [] e = return ()
updatePlayer (ActMoveBy delta:as) e@(ref, ent) = do
  setEntity ref (ent & entityPos +~ delta)
  updatePlayer as e


--
-- determineEntityActions :: World -> GameM EntityActions
-- determineEntityActions w = return IntMap.empty
--
-- determineAllEffects :: World -> EntityActions -> GameM EntityEffects
-- determineAllEffects world@(World {_entities=entities}) entityActions = do
--   let helper ref entity = determineEffects ref entity (IntMap.findWithDefault [] ref entityActions)
--   sequence $ IntMap.mapWithKey helper entities
--
-- determineEffects :: EntityRef -> Entity -> Actions -> GameM Effects
-- determineEffects _ entity =  foldM accumulator [] where
--   accumulator effectsList action = do
--     newEffects <- determineEffectsPerAction entity action
--     return (effectsList ++ newEffects)
--
-- determineEffectsPerAction :: Entity -> Action -> GameM Effects
-- determineEffectsPerAction entity (ActMoveBy delta) = return [EffMove coord] where
--   coord = (entity ^. entityPos) + delta
-- determineEffectsPerAction entity ActWait = return []
-- determineEffectsPerAction entity _ = return []
--
-- applyAllEffects :: World -> EntityEffects -> (World, EntityActions)
-- applyAllEffects world@(World {_entities=entities}) entityEffects = (world {_entities = entities'}, actions)
--   where
--       noEffects = IntMap.map $ \e -> (e, [])
--       processEffects = IntMap.mergeWithKey applyEffects noEffects (const IntMap.empty)
--       entitiesAndActions = processEffects entities entityEffects
--       entities' = IntMap.map fst entitiesAndActions
--       actions = IntMap.map snd entitiesAndActions
--
-- applyEffects :: EntityRef -> Entity -> Effects -> Maybe (Entity, Actions)
-- applyEffects _ entity effects = Just (Prelude.foldr applyEffect (entity, []) effects)
--
-- applyEffect :: Effect -> (Entity, Actions) -> (Entity, Actions)
-- applyEffect (EffMove coord) (entity, actions) = (entity', []) where
--   entity' = entity {_entityPos = coord}
-- applyEffect _ (entity, actions) = (entity, [])
