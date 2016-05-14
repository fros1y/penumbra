{-# LANGUAGE RankNTypes #-}

module ECS where

import           Data.Default
import qualified Data.Dequeue       as Dequeue
import           Data.Dynamic
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List          as List
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromJust, isJust)
import           Data.Typeable

import           Coord


type EntityRef = Int

data Component  = CRef
                | CPosition
                | CActionPoints
                | CHealth deriving (Ord, Eq, Show)

data ActionPoints = ActionPoints {
  currPoints   :: Int,
  refreshSpeed :: Int
}

instance Default ActionPoints where
  def = ActionPoints 0 0

type Entity = Map.Map Component Dynamic
type Entities = IntMap.IntMap Entity

type ComponentDynamic = (Component, Dynamic)
type ComponentValue a = (Component, a)

(#=) :: Typeable a => Component -> a -> ComponentDynamic
(#=) component value = (component, toDyn value)

(#?) :: Typeable a => Entity -> Component -> Maybe a
(#?) entity component = do
  dynamic <- Map.lookup component entity
  fromDynamic dynamic

(#) :: Typeable a => Entity -> Component -> a
(#) entity component = fromJust $ entity #? component

entityHas :: Entity -> Component -> Bool
entityHas entity component = isJust $ Map.lookup component entity

entityHasAll :: Entity -> [Component] -> Bool
entityHasAll entity = all (entityHas entity)

mkEntity :: [ComponentDynamic] -> Entity
mkEntity = Map.fromList

(#:) :: Entity -> ComponentDynamic -> Entity
(#:) entity (component, dynamic) = Map.insert component dynamic entity

(@:) :: EntityRef -> ComponentDynamic -> Entities -> Entities
(@:) entityRef cd entities = IntMap.insert entityRef entity' entities where
  entity' = entity #: cd
  entity = fromJust $ IntMap.lookup entityRef entities

entitiesWith :: Component -> Entities -> Entities
entitiesWith component = IntMap.filter (`entityHas` component)

sliceOf :: Typeable a => Component -> Entities -> IntMap.IntMap a
sliceOf component entities = IntMap.map (# component) $ entitiesWith component entities

entitiesWithAll :: [Component] -> Entities -> Entities
entitiesWithAll components = IntMap.filter (`entityHasAll` components)

data Action = AMove DeltaCoord
            | AAttack EntityRef Int
            | ADone

type Actions = [Action]

data Event = EMovement Coord
           | EDamage EntityRef Int
           | ECollision EntityRef
           | EActionPointsRecover Int
           | EActionPointsExpend Int

type Events = IntMap.IntMap [Event]
type World = Entities


render = undefined

rotate :: ActionQueue -> ActionQueue
rotate queue = queue'' where
  Just (oldFront, queue') = Dequeue.popFront queue
  queue'' = Dequeue.pushBack queue' oldFront

actionPointsToSpend :: ActionPoints -> Bool
actionPointsToSpend ap = (currPoints ap) > 0

getNextEntityToAct :: IntMap.IntMap ActionPoints -> ActionQueue -> (EntityRef, ActionQueue)
getNextEntityToAct aps queue =  if actionPointsToSpend availableAP
                                then (candidateEntityRef, queue)
                                else getNextEntityToAct aps (rotate queue)
                                  where
  (Just candidateEntityRef) = Dequeue.first queue
  availableAP = IntMap.findWithDefault def candidateEntityRef aps

getByEntityRef :: Entities -> EntityRef -> Entity
getByEntityRef ents ref = fromJust $ IntMap.lookup ref ents

setByEntityRef :: Entities -> EntityRef -> Entity -> Entities
setByEntityRef ents ref ent = IntMap.insert ref ent ents

gameLoop world queue = do
  render (observeWorld world $ getPlayerEntity world)
  let (activeEntityRef, queue') = getNextEntityToAct (sliceOf CActionPoints world) queue
      activeEntity = getByEntityRef world activeEntityRef
      observation = observeWorld world activeEntity -- handle memory?
  actions <- getEntityActions observation activeEntity
  events <- evaluateActions world actions activeEntity
  let world' = applyEvents events world
  gameLoop world' queue'

type ActionQueue = Dequeue.BankersDequeue EntityRef

playerEntityRef = 0 :: Int

getPlayerEntity :: Entities -> Entity
getPlayerEntity entities = getByEntityRef entities playerEntityRef

type ObservedWorld = CoordMap [Entity]

observeWorld :: World -> Entity -> ObservedWorld
observeWorld world entity = IntMap.foldr buildObservation mempty world

buildObservation :: Entity -> ObservedWorld -> ObservedWorld
buildObservation entity observation =
  let coord = entity # CPosition in
    case Map.lookup coord observation of
        Nothing         -> Map.insert coord [entity] observation
        (Just observed) -> Map.insert coord (entity:observed) observation

recallWorld :: World -> Entity -> ObservedWorld
recallWorld = undefined

rememberWorld :: ObservedWorld -> Entity -> Entity
rememberWorld = undefined

evaluateActions :: World -> Actions -> Entity -> IO Events
evaluateActions world actions entity = do
  events <- mapM (evaluateAction world entity) actions
  return $ mconcat events

applyEvents :: Events -> World -> World
applyEvents events world = IntMap.foldrWithKey applyEventsToRef world events

applyEventsToRef :: EntityRef -> [Event] -> World -> World
applyEventsToRef ref events world = world' where
    entity' = foldr applyEvent entity events
    entity = getByEntityRef world ref
    world' = setByEntityRef world ref entity'

--- Where the magic is

getEntityActions :: ObservedWorld -> Entity -> IO Actions
getEntityActions observation entity = return [ADone]

mkEventFor :: Entity-> Event -> Events
mkEventFor entity event = IntMap.singleton ref [event] where
  ref = entity # CRef
--
-- mkEventForAllEntities

---- EVALUATE ACITON ---
evaluateAction :: World -> Entity -> Action -> IO Events

evaluateAction _ entity ADone = return recharge where
  recharge = mkEventFor entity (EActionPointsRecover rate)
  rate = refreshSpeed (entity # CActionPoints)

evaluateAction world entity (AMove delta) = return motion where
  motion = mkEventFor entity (EMovement coord)
  coord = (entity # CPosition) + delta

evaluateAction _ _ _ = return mempty

--- APPLY EVENT---
applyEvent :: Event -> Entity -> Entity

applyEvent (EActionPointsExpend x) entity = entity #: (CActionPoints #= newAPs) where
  (ActionPoints curr _)= entity # CActionPoints
  newAPs = (entity # CActionPoints) {currPoints = curr - x}

applyEvent (EActionPointsRecover x) entity = entity #: (CActionPoints #= newAPs) where
  (ActionPoints curr _)= entity # CActionPoints
  newAPs = (entity # CActionPoints) {currPoints = curr + x}

applyEvent (EMovement coord) entity = entity #: (CPosition #= coord)

applyEvent _ entity = entity
