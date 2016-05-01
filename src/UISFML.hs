module UISFML where

import SFML.Graphics
import SFML.Window

import Prelude hiding (Either (..), (.), id)
import Control.Category
import Control.Lens
import qualified Control.Monad.State as S

import Types
import Coord
import Debug.Trace
import Data.Map.Strict as Map

initDisplay :: IO (DisplayContext)
initDisplay = do
  let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
  wnd <- createRenderWindow (VideoMode 800 600 32) "Penumbra" [SFDefaultStyle] ctxSettings
  let fontPath = "unifont.ttf"
  fnt <- err $ fontFromFile fontPath
  return DisplayContext { _wnd = wnd, _fnt = fnt }

endDisplay :: DisplayContext -> IO ()
endDisplay context = do
  destroy (context ^. wnd)
  destroy (context ^. fnt)
  return ()

screenSize :: ScreenCoord
screenSize = Coord 800 600

cellSize :: ScreenCoord
cellSize = Coord 15 10

screenSizeCells :: Coord
screenSizeCells = screenSize `Coord.quot` cellSize

celltoScreen :: Coord -> ScreenCoord
celltoScreen coord = flipOrder(coord * cellSize)

fromWorldToScreen :: WorldCoord -> (WorldCoord, a) -> IO (ScreenCoord, a)
fromWorldToScreen playerCoord (worldCoord, a) = do
  let screenMiddle = screenSizeCells `Coord.quot` 2
      offset = playerCoord - screenMiddle
  return ( celltoScreen (worldCoord - offset), a)


renderAt :: (Renderable a) => DisplayContext -> (ScreenCoord, a) -> IO ()
renderAt context (coord, a) = do
  let bounds = (Bounds origin screenSize)
      withinBounds = within coord bounds
  S.when withinBounds $ do
    txt <- err $ createText
    setTextString txt [getSymbol a]
    setTextFont txt (context ^. fnt)
    setTextCharacterSize txt $ fromInteger (cellSize ^. Types.x)
    setTextColor txt white
    let state = Just (renderStates { SFML.Graphics.transform = translation tx ty})
        tx = fromInteger (coord ^. Types.x)
        ty = fromInteger (coord ^. Types.y)
    drawText (context ^. wnd) txt $ state
    destroy txt

renderCoordMap :: (Renderable a) => DisplayContext -> WorldCoord -> CoordMap a -> IO ()
renderCoordMap context playerCoord coordMap = do
  let list = Map.toList coordMap
  mapped <- mapM (fromWorldToScreen playerCoord) list
  mapM_ (renderAt context) mapped

render :: DisplayContext -> GameM ()
render context = do
  S.liftIO $ clearRenderWindow (context ^. wnd) $ Color 0 0 0 255
  levelTiles <- use (currLevel . tiles)
  levelEntities <- use (currLevel . entities)
  playerE <- use player
  playerPos <- use playerCoord
  offsetPlayer <- S.liftIO $ fromWorldToScreen playerPos (playerPos, playerE)
  S.liftIO $ renderAt context offsetPlayer
  S.liftIO $ renderCoordMap context playerPos levelTiles
  S.liftIO $ renderCoordMap context playerPos levelEntities
  S.liftIO $ display (context ^. wnd)

getPlayerCommand :: DisplayContext -> IO (Maybe PlayerCommand)
getPlayerCommand context = do
  evt <- waitEvent (context ^. wnd)
  case evt of
    Nothing -> return Nothing
    Just SFEvtClosed -> return $ Just Quit
    Just kEvt@(SFEvtKeyPressed{}) -> return (playerCommandFromKey kEvt)
    Just _ -> return Nothing

playerCommandFromKey :: SFEvent -> Maybe PlayerCommand
playerCommandFromKey (SFEvtKeyPressed {code = keyCode,
                                      alt = altK,
                                      ctrl = ctrlK,
                                      shift = shiftK,
                                      sys = sysK}) = case (keyCode, shiftK) of
                                        (KeyUp, _) -> Just $ Go Up
                                        (KeyDown, _) -> Just $ Go Down
                                        (KeyLeft, _) -> Just $ Go Left
                                        (KeyRight, _) -> Just $ Go Right
                                        (KeyQ, False) -> Just Quit
                                        (KeyS, False) -> Just Save
                                        (KeyL, False) -> Just Load
                                        _ -> Nothing
