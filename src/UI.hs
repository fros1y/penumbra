module UI where
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as Curses
import qualified Control.Monad.State as S
import Prelude hiding (Either (..), (.), id)
import Control.Lens
import Control.Category
import Control.Monad (when)
import Data.Maybe (catMaybes, fromJust)
import Data.Map.Strict

import Types
import Coord
import Entity

renderAt :: (Renderable a) => (ScreenCoord, a) -> IO ()
renderAt (coord, a) = do
  withinBounds <- fmap (within coord) screenBounds
  when withinBounds $
    Curses.mvWAddStr Curses.stdScr (fromInteger $ coord^.x) (fromInteger $ coord^.y) $ [getSymbol a]

renderCoordMap :: (Renderable a) => WorldCoord -> CoordMap a -> IO ()
renderCoordMap playerCoord coordMap = do
  let list = toList coordMap
  mapped <- mapM (fromWorldToScreen playerCoord) list
  mapM_ renderAt mapped

render :: GameM ()
render = do
  S.liftIO Curses.erase
  levelTiles <- use (currLevel . tiles)
  levelEntities <- use (currLevel . entities)
  playerE <- use player
  playerPos <- use playerCoord
  offsetPlayer <- S.liftIO $ fromWorldToScreen playerPos (playerPos, playerE)
  S.liftIO $ renderAt offsetPlayer
  S.liftIO $ renderCoordMap playerPos levelTiles
  S.liftIO $ renderCoordMap playerPos levelEntities
  S.liftIO Curses.refresh

initDisplay :: IO ()
initDisplay = do
  Curses.start
  Curses.leaveOk True
  Curses.cursSet Curses.CursorInvisible
  Curses.erase
  Curses.refresh

endDisplay :: IO ()
endDisplay = Curses.end

playerCommandFromKey :: Curses.Key -> Maybe PlayerCommand
playerCommandFromKey k = case k of
  Curses.KeyUp -> Just $ Go Up
  Curses.KeyDown -> Just $ Go Down
  Curses.KeyLeft -> Just $ Go Left
  Curses.KeyRight -> Just $ Go Right
  Curses.KeyChar 'q' -> Just Quit
  Curses.KeyChar 's' -> Just Save
  Curses.KeyChar 'l' -> Just Load
  _ -> Nothing

getPlayerCommand :: IO (Maybe PlayerCommand)
getPlayerCommand = do
  key <- Curses.getCh
  return $ playerCommandFromKey key

screenSize :: IO Coord
screenSize = fmap toCoord Curses.scrSize where
  toCoord (x, y) = Coord (toInteger x-1) (toInteger y-1)

screenBounds :: IO Bounds
screenBounds = fmap (Bounds (Coord 0 0)) screenSize


fromWorldToScreen :: WorldCoord -> (WorldCoord, a) -> IO (ScreenCoord, a)
fromWorldToScreen playerCoord (worldCoord, a) = do
  screen <- screenSize
  let
    mx = screen^.x `quot` 2
    my = screen^.y `quot` 2
    screenMiddle = Coord mx my
  let offset = playerCoord - screenMiddle
  return (worldCoord - offset, a)
