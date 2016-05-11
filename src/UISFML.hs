{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE TemplateHaskell #-}

module UISFML where

import           Control.Category
import           Control.Lens
import qualified Control.Monad.State as S
import           Data.Colour         as Colour
import           Data.Colour.Names   as Colour
import           Data.Colour.SRGB    as Colour
import           Data.Map.Strict     as Map
import           Data.IntMap         as IntMap
import           Data.Maybe          (fromJust, isJust, isNothing)
import           Debug.Trace
import           GHC.Generics
import           Prelude             hiding (Either (..), id, (.))
import qualified SFML.Graphics       as SFML
import qualified SFML.Window         as SFML

import           Actions
import           Coord
import           Direction
import           Entity
import           GameMonad
import           Renderable
import           Symbols
import           World
import LOS
import Memory

-- DisplayContext
data DisplayContext = DisplayContext {
  _wnd   :: SFML.RenderWindow,
  _fnt   :: SFML.Font,
  _clock :: SFML.Clock
}
makeLenses ''DisplayContext

tracePipe msg value = trace (msg ++ show value) value

initDisplay :: IO DisplayContext
initDisplay = do
  let ctxSettings = Just $ SFML.ContextSettings 24 8 0 1 2 [SFML.ContextDefault]
  wnd <- SFML.createRenderWindow (SFML.VideoMode 1200 800 32) "Penumbra" [SFML.SFDefaultStyle, SFML.SFResize] ctxSettings
  SFML.setFramerateLimit wnd 60
  let fontPath = "Everson Mono.ttf"
  fnt <- SFML.err $ SFML.fontFromFile fontPath
  clk <- SFML.createClock
  return DisplayContext { _wnd = wnd, _fnt = fnt, _clock = clk}

endDisplay :: DisplayContext -> IO ()
endDisplay context = do
  SFML.destroy (context ^. wnd)
  SFML.destroy (context ^. fnt)
  return ()

screenSize :: (?context :: DisplayContext) => IO ScreenCoord
screenSize = do
  (SFML.Vec2u xsize ysize) <- SFML.getWindowSize (?context ^. wnd)
  return $ Coord (fromIntegral xsize) (fromIntegral ysize)

fontSize = 40

cellSize :: (?context :: DisplayContext) => ScreenCoord
cellSize = Coord (floor ((fromIntegral fontSize) * 0.75)) (floor ((fromIntegral fontSize) * 0.66))

screenSizeCells :: (?context :: DisplayContext) => IO Coord
screenSizeCells = do
  size <- screenSize
  let cellCount = size `Coord.quot` cellSize
  return cellCount

celltoScreen :: (?context :: DisplayContext) => Coord -> ScreenCoord
celltoScreen coord = flipOrder $ coord * cellSize

fromWorldToScreen :: (?context :: DisplayContext) => WorldCoord -> WorldCoord -> IO (ScreenCoord)
fromWorldToScreen playerCoord worldCoord = return $ celltoScreen worldCoord

convertColourToSFML :: Colour.Colour Double -> SFML.Color
convertColourToSFML c = SFML.Color r g b 255 where
  Colour.RGB r g b = Colour.toSRGB24 c

putSymbol :: (?context :: DisplayContext) => Coord -> Symbol -> SeenRemembered -> IO ()
putSymbol coord symbol seenRemembered = do
  let c = case seenRemembered of
              Seen -> (symbol ^. baseColor)
              Remembered -> Colour.red
      t = (symbol ^. glyph)
      Coord tx ty = coord
      v = SFML.Vec2f (fromIntegral tx) (fromInteger ty)
  txt <- SFML.err SFML.createText
  SFML.setTextStringU txt [t]
  SFML.setTextFont txt (?context ^. fnt)
  SFML.setTextCharacterSize txt fontSize
  SFML.setTextColor txt $ convertColourToSFML c
  SFML.setPosition txt v
  SFML.drawText (?context ^. wnd) txt (Just SFML.renderStates)
  SFML.destroy txt

renderAt :: (?context :: DisplayContext, Renderable a) => ScreenCoord -> a -> SeenRemembered -> IO ()
renderAt coord a seenRemembered = do
  let symbol = getSymbol a
      timeBased = (symbol ^. changeOverTime)
  S.when (isNothing timeBased) $ putSymbol coord symbol seenRemembered
  S.when (isJust timeBased) $ do
    time <- SFML.getElapsedTime (?context ^. clock)
    putSymbol coord ( (fromJust timeBased) time) seenRemembered

convert (SFML.Vec2u xu yu) = SFML.Vec2f (fromIntegral xu) (fromIntegral yu)
convertfromCoord (Coord xc yc) = SFML.Vec2f (fromIntegral xc) (fromIntegral yc)

seeEntity :: (?context :: DisplayContext) => (Entity -> Bool) -> Entity -> GameM ()
seeEntity lineOfSight e = do
  let pos = celltoScreen (e ^. entityPos)
  let inLine = lineOfSight e
  remembered <- inMemory e pos
  S.when inLine $ do
    remember e
    S.liftIO $ print $ "remebered: " ++ show e
    S.liftIO $ renderAt pos e Seen
  S.when (remembered && not inLine) $ do
    S.liftIO $ renderAt pos e Remembered

render :: (?context :: DisplayContext) => GameM ()
render = do
  player <- getPlayer
  ents <- use entities
  let playerPos = (player ^. entityPos)
      opaqueBuilder = buildOpaqueMapM checkCollision
      entityPoses = IntMap.map _entityPos ents
  opaqueMap <- S.foldM opaqueBuilder Map.empty entityPoses
  S.liftIO $ do
    SFML.clearRenderWindow (?context ^. wnd) $ SFML.Color 0 0 0 255
    view <- SFML.getDefaultView (?context ^. wnd)
    SFML.setViewCenter view $ convertfromCoord $ celltoScreen playerPos
    SFML.setView (?context ^. wnd) view
  mapM_ (seeEntity (entitiesInLineOfSight opaqueMap player)) ents
  S.liftIO $ SFML.display (?context ^. wnd)

handleResize :: (?context :: DisplayContext) => Int -> Int -> IO ()
handleResize w h = do
  SFML.setWindowSize (?context ^. wnd) (SFML.Vec2u (fromIntegral w) (fromIntegral h))
  view <- SFML.getDefaultView (?context ^. wnd)
  SFML.resetView view (SFML.FloatRect 0 0 (fromIntegral w) (fromIntegral h))
  SFML.setView (?context ^. wnd) view
  return ()
