import Prelude hiding (Either (..), (.), id)
import Data.Label
import Control.Category
import qualified Control.Monad.State as S
import Control.Monad.Random

import Types
import UI
import Coord

import Control.Monad (unless)

moveEntity :: Entity -> Direction -> Entity
moveEntity e d = set position position' e where
  position' = get position e + fromDirection d

mkEntity :: Char -> Coord -> Entity
mkEntity sym pos = Entity pos sym Nothing

mkRandomLevel :: Bounds -> GameM Level
mkRandomLevel bounds = do
  let boundary = borderCoords bounds
  randomPillarLocations <- S.forM [1 .. 20] $ \_i -> randomWithin bounds
  return $ map (mkEntity '#') (randomPillarLocations ++ boundary)

main :: IO ()
main = do
  initDisplay
  let world = World (mkEntity '@' (Coord 0 0)) []
  S.runStateT setup world
  endDisplay

update :: World -> Maybe PlayerCommand -> World
update = updatePlayer

updatePlayer :: World -> Maybe PlayerCommand -> World
updatePlayer w Nothing = w
updatePlayer w (Just (Go d)) = set player player' w where
   player' = moveEntity (get player w) d

setup :: GameM ()
setup = do
  world <- S.get
  S.liftIO (setStdGen $ mkStdGen 1)
  randomLevel <- mkRandomLevel $ Bounds origin (Coord 20 40)
  S.put $ set currLevel randomLevel world
  gameLoop

gameLoop :: GameM ()
gameLoop = do
  world <- S.get
  S.liftIO $ render world
  command <- S.liftIO getPlayerCommand
  S.put $ update world command
  unless (command == Just Quit) gameLoop
