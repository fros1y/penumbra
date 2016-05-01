{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where
import Prelude hiding (Either (..), (.), id)
import Control.Lens
import qualified Control.Monad.State as S
import Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Control.Category
import GHC.Generics
import Data.Default
import SFML.Graphics
import SFML.Window

data DisplayContext = DisplayContext {
  _wnd :: RenderWindow,
  _fnt :: Font
}

data Direction = Up | Down | Left | Right deriving (Show, Read, Eq, Generic)

type GameM = S.StateT World IO

data Coord = Coord {
  _x :: Integer,
  _y :: Integer
  } deriving (Eq, Show, Read, Ord, Generic)

instance Default Coord where
  def = Coord 0 0


type WorldCoord = Coord
type ScreenCoord = Coord

data Bounds = Bounds {
  upper :: Coord,
  lower :: Coord
} deriving (Eq, Show, Read, Ord, Generic)

instance Default Bounds where
  def = Bounds def def

data Entity = GenericEntity |
              Player Specifics |
              Rat Specifics deriving (Show, Read, Eq, Generic)

instance Renderable Entity where
  getSymbol GenericEntity = '?'
  getSymbol (Player _) = '@'
  getSymbol (Rat _)    = 'r'

instance Default Entity where
  def = GenericEntity

data Level = Level {
  _tiles :: TileMap,
  _entities :: EntityMap,
  _bounds :: Bounds
} deriving (Show, Read, Generic)

instance Default Types.Level where
  def = Level def def def

data World = World {
  _turnCount :: Integer,
  _player :: Entity,
  _playerCoord :: WorldCoord,
  _currLevel :: Types.Level
} deriving (Show, Read, Generic)

instance Default World where
  def = World 0 player def def where
    player = Player def

type Symbol = Char

data PlayerCommand  = Go Direction
                    | Save
                    | Load
                    | Quit deriving (Show, Read, Eq, Generic)

type CoordMap a = Map.Map Coord a
type TileMap = CoordMap (Maybe Tile)
type EntityMap = CoordMap (Maybe Entity)

class Renderable a where
  getSymbol :: a -> Symbol

class Obstructor a where
  isObstructing :: a -> Bool
  isOpaque :: a -> Bool

instance Obstructor Tile where
  isObstructing _ = True
  isOpaque _ = True

data Specifics = Specifics {
  _name :: Maybe String,
  _healthPoints :: Maybe Int
} deriving (Show, Read, Eq, Generic)

instance Default Specifics where
  def = Specifics Nothing Nothing

data Tile =  Floor Specifics
          |  Wall Specifics
          |  Pillar Specifics deriving (Show, Read, Eq, Generic)

instance Default Tile where
  def = Floor def

instance (Renderable a) => Renderable (Maybe a) where
  getSymbol (Just a) = getSymbol a
  getSymbol _ = ' '

instance Renderable Tile where
  getSymbol (Floor _) = '.'
  getSymbol (Wall _) = '#'
  getSymbol (Pillar _) = '|'

makeLenses ''World
makeLenses ''Coord
makeLenses ''Types.Level
makeLenses ''Specifics
makeLenses ''DisplayContext
