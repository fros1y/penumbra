{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Types where
import Prelude hiding (Either (..), (.), id)
import Control.Lens
import qualified Control.Monad.State as S
import Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Control.Category

data Direction = Up | Down | Left | Right deriving (Show, Eq)

type GameM = S.StateT World IO

data Coord = Coord {
  _x :: Integer,
  _y :: Integer
  } deriving (Eq, Show, Ord)

data Bounds = Bounds {
  upper :: Coord,
  lower :: Coord
} deriving (Eq, Show, Ord)

type SymbolDisplay = (Char, Coord)

data Damageable = Damageable {
  _maxHealth :: Int,
  _currHealth :: Int
} deriving Show

data EntityType = Player
                | Monster
                deriving (Show, Eq)

type Symbol = Char

data Obstruction = Obstruction {
  _transparent :: Bool,
  _flyOver :: Bool
} deriving Show

data Behavior =   PassiveBehavior
                | BrainlessBehavior
                deriving (Show, Eq)

data Entity = Entity {
  _entityID :: EntityID,
  _position :: Coord,
  _symbol :: Symbol,
  _damageable :: Maybe Damageable,
  _obstruction :: Maybe Obstruction,
  _behavior :: Maybe Behavior,
  _entityType :: EntityType
} deriving Show

data Level = Level {
  _entityIDs :: [EntityID],
  _bounds :: Bounds
} deriving Show

type EntityID = Integer
type EntityMap a = Map.Map EntityID a

type EntityBuilder = Entity -> Entity

data World = World {
  _turnCount :: Integer,
  _player :: EntityID,
  _currLevel :: Types.Level,
  _nextEntityID :: EntityID,
  _allEntities :: EntityMap Entity
} deriving Show

data PlayerCommand  = Go Direction
                    | Quit deriving (Show, Eq)






makeLenses ''Entity
makeLenses ''Obstruction
makeLenses ''Damageable
makeLenses ''World
makeLenses ''Coord
makeLenses ''Types.Level
