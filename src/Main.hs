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
-- import Entity
-- import Level
-- import State

import Data.Maybe (fromJust)


addEntities :: [(EntityRef, Entity)] -> World -> World
addEntities list w = Prelude.foldr addEntityAt w list

addEntityAt :: (EntityRef, Entity) -> World -> World
addEntityAt (eref, e) w = w & entities %~ insert where
  insert = IntMap.insert eref e

mkWall :: Coord -> Entity
mkWall coord = Entity Wall coord True

mkPlayerAction :: Action -> EntityActions
mkPlayerAction a = IntMap.singleton 0 [a]

getUserAction :: (?context :: DisplayContext) => PlayerCommand -> EntityActions
getUserAction (Go direction) = mkPlayerAction (ActMoveBy (fromDirection direction))
getUserAction _ = IntMap.empty

collectActions :: EntityActions -> EntityActions -> EntityActions
collectActions = IntMap.unionWith (++)

main :: IO ()
main = do
  context' <- initDisplay
  let ?context = context' in setup
  endDisplay context'

setup :: (?context :: DisplayContext) => GameM ()
setup = do
  let w = addEntities [ (1, mkWall (Coord 4 4)),
                        (2, mkWall (Coord 3 4))]
                      def
  gameLoop w IntMap.empty
  return ()

gameLoop :: (?context :: DisplayContext) => World -> EntityActions -> GameM ()
gameLoop w actions = do
  (w', replyActions) <- updateWorld w actions
  render w'
  command <- getPlayerCommand
  case command of
    Nothing -> gameLoop w actions
    Just Quit -> return ()
    Just Save -> do
      let worldSave = w & pendingActions .~ actions
      -- S.liftIO $ saveState state
      gameLoop w actions
    Just Load -> do
      -- state <- S.liftIO loadState
      -- S.put (fromJust state)
      gameLoop w actions
    Just userCommand -> do
      let userAction = getUserAction userCommand
          actions' = collectActions replyActions userAction
      gameLoop w' actions'

updateWorld :: World -> EntityActions -> GameM (UpdatedWorld, EntityActions)
updateWorld world@(World {_entities=entities}) actions = do
  newActions <- determineEntityActions world
  effects <- determineAllEffects world (collectActions actions newActions)
  let (world', resultingActions) = applyAllEffects world effects
  return (world', resultingActions)

determineEntityActions :: World -> GameM EntityActions
determineEntityActions w = return IntMap.empty

determineAllEffects :: World -> EntityActions -> GameM EntityEffects
determineAllEffects world@(World {_entities=entities}) entityActions = do
  let helper ref entity = determineEffects ref entity (IntMap.findWithDefault [] ref entityActions)
  sequence $ IntMap.mapWithKey helper entities

determineEffects :: EntityRef -> Entity -> Actions -> GameM Effects
determineEffects _ entity =  foldM accumulator [] where
  accumulator effectsList action = do
    newEffects <- determineEffectsPerAction entity action
    return (effectsList ++ newEffects)

determineEffectsPerAction :: Entity -> Action -> GameM Effects
determineEffectsPerAction entity (ActMoveBy delta) = return [EffMove coord] where
  coord = (entity ^. entityPos) + delta
determineEffectsPerAction entity ActWait = return []
determineEffectsPerAction entity _ = return []

applyAllEffects :: World -> EntityEffects -> (World, EntityActions)
applyAllEffects world@(World {_entities=entities}) entityEffects = (world {_entities = entities'}, actions)
  where
      noEffects = IntMap.map $ \e -> (e, [])
      processEffects = IntMap.mergeWithKey applyEffects noEffects (const IntMap.empty)
      entitiesAndActions = processEffects entities entityEffects
      entities' = IntMap.map fst entitiesAndActions
      actions = IntMap.map snd entitiesAndActions

applyEffects :: EntityRef -> Entity -> Effects -> Maybe (Entity, Actions)
applyEffects _ entity effects = Just (Prelude.foldr applyEffect (entity, []) effects)

applyEffect :: Effect -> (Entity, Actions) -> (Entity, Actions)
applyEffect (EffMove coord) (entity, actions) = (entity', []) where
  entity' = entity {_entityPos = coord} 
applyEffect _ (entity, actions) = (entity, [])
