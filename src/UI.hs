module UI where
import Types
import Coord

import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as Curses

import Prelude hiding (Either (..), (.), id)
import Data.Label
import Control.Category
import Control.Monad (when)

drawCharAt :: SymbolDisplay -> IO ()
drawCharAt (char, screenCoord) = do
  withinBounds <- fmap (within screenCoord) screenBounds
  when withinBounds $ Curses.mvWAddStr Curses.stdScr (fromInteger $ x screenCoord) (fromInteger $ y screenCoord) [char]

renderEntity :: Entity -> SymbolDisplay
renderEntity e = (get symbol e, get position e)

render :: World -> IO ()
render world = do
  Curses.erase
  let symbols = map renderEntity ((get player world):(get currLevel world))
  let playerPos = (get (position . player) world)
      shift = fromWorldToScreen playerPos
  screenShifted <- mapM shift symbols
  mapM_ drawCharAt screenShifted
  Curses.refresh

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


fromWorldToScreen :: Coord -> SymbolDisplay -> IO SymbolDisplay
fromWorldToScreen playerCoord (symbol, worldCoord) = do
  screen <- screenSize
  let
    mx = (x screen) `quot` 2
    my = (y screen) `quot` 2
    screenMiddle = Coord mx my
  let offset = playerCoord - screenMiddle
  return (symbol, worldCoord - offset)
