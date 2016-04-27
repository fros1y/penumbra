{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Types where
import Prelude hiding (Either (..), (.), id)
import Data.Label
import qualified Control.Monad.State as S

data Direction = Up | Down | Left | Right deriving (Show, Eq)

type GameM = S.StateT World IO

data Coord = Coord {
  x :: Integer,
  y :: Integer
  } deriving (Eq, Show, Ord)

type SymbolDisplay = (Char, Coord)

data Health = Health {
  _maxHealth :: Int,
  _currHealth :: Int
} deriving Show

data Entity = Entity {
  _position :: Coord,
  _symbol :: Char,
  _health :: Maybe Health
} deriving Show

type Level = [Entity]

data World = World {
  _player :: Entity,
  _currLevel :: Level
} deriving Show

data PlayerCommand  = Go Direction
                    | Quit deriving (Show, Eq)

mkLabels [''Entity, ''Health, ''World]
