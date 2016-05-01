{-# LANGUAGE ImplicitParams #-}
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
  wnd <- createRenderWindow (VideoMode 1200 800 32) "Penumbra" [SFDefaultStyle, SFResize] ctxSettings
  setFramerateLimit wnd 60
  let fontPath = "unifont.ttf"
  fnt <- err $ fontFromFile fontPath
  return DisplayContext { _wnd = wnd, _fnt = fnt }

endDisplay :: DisplayContext -> IO ()
endDisplay context = do
  destroy (context ^. wnd)
  destroy (context ^. fnt)
  return ()

screenSize :: (?context :: DisplayContext) => IO (ScreenCoord)
screenSize = do
  (Vec2u xsize ysize) <- getWindowSize (?context ^. wnd)
  return $ Coord (fromIntegral xsize) (fromIntegral ysize)

cellSize :: (?context :: DisplayContext) => ScreenCoord
cellSize = Coord 15 10

screenSizeCells :: (?context :: DisplayContext) => IO (Coord)
screenSizeCells = do
  size <- screenSize
  let cellCount = size `Coord.quot` cellSize
  return $ cellCount

celltoScreen :: (?context :: DisplayContext) => Coord -> ScreenCoord
celltoScreen coord = flipOrder(coord * cellSize)

fromWorldToScreen :: (?context :: DisplayContext) => WorldCoord -> (WorldCoord, a) -> IO (ScreenCoord, a)
fromWorldToScreen playerCoord (worldCoord, a) = return ( celltoScreen worldCoord, a)
-- fromWorldToScreen playerCoord (worldCoord, a) = do
--   sizeCells <- screenSizeCells
--   traceIO $ "screenSizeCells: " ++ show sizeCells
--   let screenMiddle = sizeCells `Coord.quot` (Coord 2 2)
--       offset = playerCoord - screenMiddle
--   traceIO $ "screenMiddle: " ++ show screenMiddle
--   return ( celltoScreen (worldCoord - offset), a)


renderAt :: (?context :: DisplayContext, Renderable a) => DisplayContext -> (ScreenCoord, a) -> IO ()
renderAt context (coord, a) = do
  size <- screenSize
  let bounds = (Bounds origin size)
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

renderCoordMap :: (?context :: DisplayContext, Renderable a) => DisplayContext -> WorldCoord -> CoordMap a -> IO ()
renderCoordMap context playerCoord coordMap = do
  let list = Map.toList coordMap
  mapped <- mapM (fromWorldToScreen playerCoord) list
  mapM_ (renderAt context) mapped


convert (Vec2u xu yu) = Vec2f (fromIntegral xu) (fromIntegral yu)

render :: (?context :: DisplayContext) => GameM ()
render = do
  S.liftIO $ clearRenderWindow (?context ^. wnd) $ Color 0 0 0 255
  levelTiles <- use (currLevel . tiles)
  levelEntities <- use (currLevel . entities)
  playerE <- use player
  playerPos <- use playerCoord
  offsetPlayer <- S.liftIO $ fromWorldToScreen playerPos (playerPos, playerE)
  S.liftIO $ renderAt ?context offsetPlayer
  S.liftIO $ renderCoordMap ?context playerPos levelTiles
  S.liftIO $ renderCoordMap ?context playerPos levelEntities
  S.liftIO $ display (?context ^. wnd)

handleResize :: (?context :: DisplayContext) => Int -> Int -> IO ()
handleResize w h = do
  setWindowSize (?context ^. wnd) (Vec2u (fromIntegral w) (fromIntegral h))
  view <- createView
  resetView view (FloatRect 0 0 (fromIntegral w) (fromIntegral h))
  setView (?context ^. wnd) view
  return ()


getPlayerCommand :: (?context :: DisplayContext) => IO (Maybe PlayerCommand)
getPlayerCommand = do
  evt <- waitEvent (?context ^. wnd)
  case evt of
    Nothing -> return Nothing
    Just SFEvtClosed -> return $ Just Quit
    Just SFEvtResized {width = w, height = h} -> do
      handleResize w h
      return Nothing
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
