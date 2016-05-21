{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

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
import           Data.Maybe           (fromJust)
import           Debug.Trace
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Auto.Blip.Internal

import Data.Serialize
import GHC.Generics

import           Data.Colour         as Colour
import           Data.Colour.Names   as Colour
import           Data.Colour.SRGB    as Colour
import qualified SFML.Graphics       as SFML
import qualified SFML.Window         as SFML

import           Actions
import           Coord
import           Entity
import           GameMonad
import           Level
import           Obstruction
import           PlayerCommand
import           Serialize
import           State
import           UISFML

import Symbols
import Memory

getUserInput :: (?context :: DisplayContext) => IO Actions
getUserInput = do
  userCommand <- getPlayerCommand
  case userCommand of
    Nothing -> getUserInput
    (Just cmd) -> return $ getUserAction cmd

getUserAction :: (?context :: DisplayContext) => PlayerCommand -> Actions
getUserAction (Go direction) = [ActMoveBy (fromDirection direction)]
getUserAction _ = []

-- renderer :: (?context :: DisplayContext) => P.Proxy Actions y' y IO ()
-- renderer = S.forever loop where
--   loop = do
--     liftIO $ print action
--     liftIO $ SFML.clearRenderWindow (?context ^. wnd) $ SFML.Color 0 0 0 255
--     liftIO $ putSymbol (Coord 100 100) (Symbol '@' Colour.white Nothing) Seen
--     S.liftIO $ SFML.display (?context ^. wnd)
--     loop


getMovement :: Auto' (Maybe PlayerCommand) (Blip Coord)
getMovement = emitJusts isMovement where
  isMovement input = case input of
    (Just (Go direction)) -> Just (fromDirection direction)
    _ -> Nothing

countTurn = accumB (\b _ -> b + 1) (0 :: Int)

instance Serialize Coord

mkPosition initialPos = holdWith initialPos . perBlip (sumFrom initialPos) . getMovement

data EntityState = EntityState {
  _position :: Coord,
  _symbol :: Symbol,
  _health :: Int
} deriving (Generic, Show)

data GameState = GameState {
  _turnCount :: Int,
  _gameEntities :: IntMap EntityState
} deriving (Generic, Show)

instance Serialize GameState
instance Serialize EntityState
instance Serialize Symbol
instance Serialize (Colour Double) where
  put = put . show
  get = read <$> get

data Effect = Damage Int
            | Heal Int
            | PlayerMove DeltaCoord
            | Pass

type Effects = [Effect]
type ObservedWorld = [EntityState]


updateHealth :: Int -> Int -> Effect -> Int
updateHealth maxHP currHP (Damage x) = currHP - x
updateHealth maxHP currHP (Heal x) = max (currHP + x) maxHP
updateHealth maxHP currHP _ = currHP

updatePosition :: Coord -> Effect -> Coord
updatePosition currPos (PlayerMove delta) = currPos + delta
updatePosition currPos _ = currPos

type EntityInput = (Blip Effect, GameState)

mkEntity startingPos symbol startingHealth maxHealth = entity where
  entity :: Interval' EntityInput EntityState
  entity = proc (effect, observedWorld) -> do

    health <- holdWith startingHealth . accumB (updateHealth maxHealth) startingHealth -< effect

    position <- holdWith startingPos . accumB updatePosition startingPos -< effect

    id -< if health > 0 then Just (EntityState position symbol health) else Nothing

playerSymbol = (Symbol '@' Colour.white)
wallSymbol = (Symbol '#' Colour.gray)

spawner :: Int -> Interval' EntityInput EntityState
spawner k = nothing where
  nothing :: Interval' EntityInput EntityState
  nothing = proc _ -> do
    id -< Nothing

buildGameStateFromPair :: (IntMap EntityState, Int) -> GameState
buildGameStateFromPair (entities, turnCount) = GameState turnCount entities

mkGame initialGameState = game where
  game :: Auto' (Blip Effect) GameState
  game = proc playerEffect -> do
    rec gameState <- lastVal initialGameState -< gameState'
        objectCreation <- never -< ()
        entities <- dynMapF spawner (NoBlip, GameState {}) -< (IntMap.singleton 0 (playerEffect, gameState), objectCreation)
        turnCount <- iterator (+ 1) 0 -< playerEffect
        gameState' <- arr buildGameStateFromPair -< (entities, turnCount)
    id -< gameState'

main :: IO ()
main = do
  context' <- initDisplay
  let ?context = context' in runGameAuto mkGame
  endDisplay context'

runGameAuto = undefined
-- runGameAuto auto = do
--   command <- getPlayerCommand
--   let (gameState, nextAuto) = stepAuto' auto command
--   print gameState
--   runGameAuto nextAuto

--
-- main :: IO ()
-- main = do
--   context' <- initDisplay
--   let ?context = context' in (doGame setup def)
--   endDisplay context'
--
-- setup :: (?context :: DisplayContext) => GameM ()
-- setup = do
--   let player = Entity {_entityType=Player, _entityPos=Coord 20 20, _entityAlive=True, _lightSource = Nothing}
--   addEntityAt (0, player)
--   level <- mkRandomLevel (Bounds origin (Coord 40 40))
--   addEntities level
--   gameLoop
--   return ()
--
-- gameLoop :: (?context :: DisplayContext) => GameM ()
-- gameLoop = do
--   render
--   command <- S.liftIO getPlayerCommand
--   case command of
--     Nothing -> gameLoop
--     Just Quit -> return ()
--     Just Save -> do
--       saveWorld <- getWorld
--       S.liftIO $ saveState saveWorld
--       gameLoop
--     Just Load -> do
--       loadWorld <- S.liftIO $ loadState
--       setWorld (fromJust loadWorld)
--       gameLoop
--     Just userCommand -> do
--       let userAction = getUserAction userCommand
--       updateWorld userAction
--       gameLoop
--
-- updateWorld :: Actions -> GameM ()
-- updateWorld actions = do
--   ents <- use entities
--   let moveOrder = IntMap.map (_entityRef) ents
--   mapM_ (updateEntity actions) moveOrder
--
-- updateEntity :: Actions -> EntityRef -> GameM ()
-- updateEntity actions 0 = updatePlayer actions
-- updateEntity _ _ = return ()
--
-- updatePlayer :: Actions -> GameM ()
-- updatePlayer (ActMoveBy delta:as) = do
--   moveCheckingForCollision delta 0
--   updatePlayer as
-- updatePlayer [] = return ()
