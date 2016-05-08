{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DeriveGeneric     #-}


module PlayerCommand where
  import           Control.Category
  import           Control.Lens
  import qualified Control.Monad.State as S
  import           Prelude             hiding (Either (..), id, (.))
  import qualified SFML.Graphics       as SFML
  import qualified SFML.Window         as SFML
  import           Data.Map.Strict     as Map
  import           Debug.Trace
  import           GHC.Generics
  import           Data.Colour         as Colour
  import           Data.Colour.Names   as Colour
  import           Data.Colour.SRGB    as Colour
  import           Data.Maybe          (fromJust, isJust, isNothing)

  import           GameMonad
  import           Coord
  import           Entity
  import Actions
  import Symbols
  import Renderable
  import Direction
  import World
  import UISFML

  data PlayerCommand  = Go Direction
                      | Save
                      | Load
                      | Quit deriving (Show, Read, Eq, Generic)

  getPlayerCommand :: (?context :: DisplayContext) => IO (Maybe PlayerCommand)
  getPlayerCommand = do
    evt <- SFML.pollEvent (?context ^. wnd)
    case evt of
      Nothing -> return Nothing
      Just SFML.SFEvtClosed -> return $ Just Quit
      Just SFML.SFEvtResized {SFML.width = w, SFML.height = h} -> do
        handleResize w h
        return Nothing
      Just kEvt@SFML.SFEvtKeyPressed{} -> return (playerCommandFromKey kEvt)
      Just _ -> return Nothing

  playerCommandFromKey :: SFML.SFEvent -> Maybe PlayerCommand
  playerCommandFromKey SFML.SFEvtKeyPressed {SFML.code = keyCode,
                                        SFML.alt = altK,
                                        SFML.ctrl = ctrlK,
                                        SFML.shift = shiftK,
                                        SFML.sys = sysK} = case (keyCode, shiftK) of
                                          (SFML.KeyUp, _) -> Just $ Go Up
                                          (SFML.KeyDown, _) -> Just $ Go Down
                                          (SFML.KeyLeft, _) -> Just $ Go Left
                                          (SFML.KeyRight, _) -> Just $ Go Right
                                          (SFML.KeyQ, False) -> Just Quit
                                          (SFML.KeyS, False) -> Just Save
                                          (SFML.KeyL, False) -> Just Load
                                          _ -> Nothing
