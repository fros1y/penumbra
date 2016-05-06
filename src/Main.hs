{-# LANGUAGE ImplicitParams #-}

import Prelude hiding (Either (..), (.), id)
import Control.Category
import Control.Lens

import qualified Control.Monad.State as S
import qualified Control.Monad.Random as Random
import Control.Monad (unless)
import Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import Data.Default

import Serialize
import Types
import UISFML
import Coord
import Entity
import Level
import State

import Data.Maybe (fromJust)

main :: IO ()
main = do
  context' <- initDisplay
  let ?context = context' in setup def
  endDisplay context'

update :: Maybe PlayerCommand -> World -> GameM (World)
update Nothing world = return world
update (Just command) world = do
  let playerCoord' = updatePlayer command (world ^. playerCoord)
  let world' = world {_playerCoord = playerCoord'}
  return world'

updatePlayer :: PlayerCommand -> Coord -> Coord
updatePlayer (Go d) coord = coord + fromDirection d
updatePlayer _ coord = coord

setup :: (?context :: DisplayContext) => World -> GameM ()
setup world = do
  Random.setStdGen $ Random.mkStdGen 1
  clevel <- mkRandomLevel $ Bounds origin (Coord 100 100)
  let world' = world { _currLevel = clevel, _playerCoord = Coord 10 10, _turnCount = 0 }
  gameLoop world'

gameLoop :: (?context :: DisplayContext) => World -> GameM ()
gameLoop world = do
  render world
  command <- getPlayerCommand
  case command of
    Just Quit -> return ()
    Just Save -> do
      saveState world
      gameLoop world
    Just Load -> do
      world' <- fromJust <$> loadState
      gameLoop world'
    Nothing -> gameLoop world
    _ -> do
      world' <- Main.update command world
      gameLoop world'
