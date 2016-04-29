module UI where
import Types
import Coord

import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as Curses
import qualified Control.Monad.State as S
import Prelude hiding (Either (..), (.), id)
import Control.Lens
import Control.Category
import Control.Monad (when)

import Data.Maybe (catMaybes, fromJust)

drawCharAt :: SymbolDisplay -> IO ()
drawCharAt (char, screenCoord) = do
  withinBounds <- fmap (within screenCoord) screenBounds
  when withinBounds $ Curses.mvWAddStr Curses.stdScr (fromInteger $ screenCoord^.x) (fromInteger $ screenCoord^.y) [char]

renderEntity :: Entity -> SymbolDisplay
renderEntity e = (e^.symbol, e^.position)

render :: GameM ()
render = do
  S.liftIO $ Curses.erase
  playerEntityID <- use player
  playerEntity <- lookupEntitybyID_ playerEntityID
  levelEntityIDs <- use (currLevel . entityIDs)
  entities <- mapM lookupEntitybyID levelEntityIDs
  let symbols = map renderEntity $ playerEntity:catMaybes entities
      playerPos =  playerEntity ^. position
      shift = fromWorldToScreen playerPos
  screenShifted <- S.liftIO $ mapM shift symbols
  S.liftIO $ mapM_ drawCharAt screenShifted
  S.liftIO $ Curses.refresh

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
    mx = screen^.x `quot` 2
    my = screen^.y `quot` 2
    screenMiddle = Coord mx my
  let offset = playerCoord - screenMiddle
  return (symbol, worldCoord - offset)
