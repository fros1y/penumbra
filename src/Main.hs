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
import           Data.Maybe           (fromJust)
import           Debug.Trace

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
import           World

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
  let player = Entity {_entityType=Player, _entityPos=Coord 20 20, _entityAlive=True, _lightSource = Nothing}
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
  let moveOrder = IntMap.map (_entityRef) ents
  mapM_ (updateEntity actions) moveOrder

updateEntity :: Actions -> EntityRef -> GameM ()
updateEntity actions 0 = updatePlayer actions
updateEntity _ _ = return ()

updatePlayer :: Actions -> GameM ()
updatePlayer (ActMoveBy delta:as) = do
  moveCheckingForCollision delta 0
  updatePlayer as
updatePlayer [] = return ()
