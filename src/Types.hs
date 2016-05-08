{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Types where
import           Prelude             hiding (Either (..), id, (.))

import           Control.Category
import           Control.Lens
import qualified Control.Monad.State as S
import           Data.Map.Strict     as Map
import           Data.Maybe          (fromJust)

import           Data.Default
import           GHC.Generics

import           Data.Colour         as Colour
import           Data.Colour.Names   as Colour
import qualified SFML.Graphics       as SFML
import qualified SFML.Window         as SFML

import           Data.IntMap.Strict  as IntMap
import           Debug.Trace

data DisplayContext = DisplayContext {
  _wnd   :: SFML.RenderWindow,
  _fnt   :: SFML.Font,
  _clock :: SFML.Clock
}

data Direction = Up | Down | Left | Right deriving (Show, Read, Eq, Generic)

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

type EntityRef = Int
type DeltaCoord = Coord

data EntityType = Player |
                  Rat |
                  Frog |
                  Wall |
                  Floor
                  deriving (Show, Eq, Generic)

data Entity = Entity {
  _entityType  :: EntityType,
  _entityPos   :: Coord,
  _entityAlive :: Bool
} deriving (Show, Generic)


type UpdatedEntity = Entity

type TargetEntityRef = EntityRef
type SourceEntityRef = EntityRef

data Action = ActWait |
              ActMoveBy DeltaCoord |
              ActAttack TargetEntityRef
              deriving (Show, Generic, Eq)

type Actions = [Action]

type EntityRefMap a = IntMap.IntMap a
type EntityActions = EntityRefMap Actions
type Entities = EntityRefMap Entity

type TurnCount = Int

data World = World {
  _entities       :: Entities,
  _turnCount      :: TurnCount,
  _pendingActions :: EntityActions,
  _nextEntityRef  :: EntityRef
} deriving (Show, Generic)
type UpdatedWorld = World

instance Default World where
  def = World (IntMap.singleton 0 (Entity Player def True)) 0 IntMap.empty 1

data Symbol = Symbol {
  _glyph          :: Char,
  _baseColor      :: Colour.Colour Double,
  _changeOverTime :: Maybe (SFML.Time -> Symbol)
} deriving (Generic)

instance Default Symbol where
  def = Symbol '?' Colour.white Nothing

data PlayerCommand  = Go Direction
                    | Save
                    | Load
                    | Quit deriving (Show, Read, Eq, Generic)


makeLenses ''Coord
makeLenses ''DisplayContext
makeLenses ''Symbol
makeLenses ''Entity
makeLenses ''World

class Obstruction a where
  obstructs :: a -> Bool

instance Obstruction Entity where
  obstructs entity = obstructs (entity ^. entityType)

instance Obstruction EntityType where
  obstructs Floor = False
  obstructs _ = True

class Renderable a where
  getSymbol :: a -> Symbol

instance (Renderable a) => Renderable (Maybe a) where
  getSymbol (Just a) = getSymbol a
  getSymbol _ = def

instance Renderable Entity where
  getSymbol entity = getSymbol (entity ^. entityType)

instance Renderable EntityType where
  getSymbol Player  = Symbol '@' Colour.white Nothing
  getSymbol Floor   = Symbol '·' Colour.dimgray Nothing
  getSymbol Rat     = Symbol 'r' Colour.brown Nothing
  getSymbol Wall    = Symbol '#' Colour.white Nothing
  getSymbol _       = Symbol '?' Colour.dimgray Nothing

flicker :: SFML.Time -> Symbol
flicker t = Symbol '◯' color Nothing where
  one = Colour.yellow
  two = Colour.red
  blend = abs ( sin (fromIntegral (SFML.asMilliseconds t)  / 1000) )
  color = Colour.blend blend one two
