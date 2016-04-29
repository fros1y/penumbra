module Coord where

import Prelude hiding (Either (..), (.), id)
import Control.Monad.Random

import Types

instance Num Coord where
  (+) (Coord x y) (Coord x' y') = Coord (x+x') (y+y')
  (*) (Coord x y) (Coord x' y') = Coord (x*x') (y*y')
  negate (Coord x y) = Coord (-x) (-y)
  abs (Coord x y) = Coord (abs x) (abs y)
  signum (Coord x y) = Coord (signum x) (signum y)
  fromInteger i = Coord i' i' where
    i' = fromInteger i

toPair :: Coord -> (Integer, Integer)
toPair (Coord x y) = (x, y)

fromPair :: (Integer, Integer) -> Coord
fromPair (x, y) = Coord x y

coordsWithin :: Bounds -> [Coord]
coordsWithin (Bounds (Coord lx ly) (Coord ux uy)) =
  [Coord x y | x<-[lx..ux], y<-[ly..uy]]

borderCoords :: Bounds -> [Coord]
borderCoords (Bounds (Coord lx ly) (Coord ux uy)) =
  [Coord x y | x<-[lx..ux], y<-[ly..uy], x == lx || x == ux || y == ly || y == uy]

within :: Coord -> Bounds -> Bool
within c bounds = c `elem` coordsWithin bounds

randomWithin :: MonadRandom m => Bounds -> m Coord
randomWithin b = uniform (coordsWithin b)

fromDirection :: Direction -> Coord
fromDirection d = case d of
  Left    -> Coord 0 (-1)
  Right  -> Coord 0 1
  Up  -> Coord (-1) 0
  Down -> Coord 1 0

origin :: Coord
origin = Coord 0 0
