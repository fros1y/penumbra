
module LOS where

import qualified Data.Map.Lazy as Map
import Data.Maybe (isJust)

import Coord
import GameMonad
import Entity

data Opaque = Opaque deriving Show
type OpaqueMap = CoordMap (Maybe Opaque)

isOpaque :: OpaqueMap -> Coord -> Bool
isOpaque opaqueMap coord = isJust $ Map.lookup coord opaqueMap


buildOpaqueMapM :: (Coord -> GameM Bool) -> OpaqueMap -> Coord -> GameM OpaqueMap
buildOpaqueMapM getOpaqueM existingMap coord = do
  opaque <- getOpaqueM coord
  return $  if opaque
            then Map.insert  coord (Just Opaque) existingMap
            else existingMap

buildOpaqueMap :: (Coord -> Bool) -> Coord -> OpaqueMap -> OpaqueMap
buildOpaqueMap getOpaque coord existingMap =  if getOpaque coord
                                              then Map.insert  coord (Just Opaque) existingMap
                                              else existingMap

inLineOfSight :: OpaqueMap -> Coord -> Coord -> Bool
inLineOfSight opaqueMap
              coord2 coord1 = if (coord1 == coord2) || (nextCoord == coord2) then True
                              else (not $ isOpaque opaqueMap nextCoord) &&
                                    inLineOfSight opaqueMap nextCoord coord2 where
                                      nextCoord = nextInLine coord1 coord2

entitiesInLineOfSight :: OpaqueMap -> Entity -> Entity -> Bool
entitiesInLineOfSight opaqueMap e1 e2 = inLineOfSight opaqueMap (_entityPos e1) (_entityPos e2)

nextInLine :: Coord -> Coord -> Coord
nextInLine c1 c2 = head $ lineCoords c1 c2

lineCoords :: Coord -> Coord -> [Coord]
lineCoords c1 c2 = map fromPair $ (tail $ bla (toPair c1) (toPair c2))

-- | Bresenham's line algorithm. http://www.roguebasin.com/index.php?title=Bresenham%27s_Line_Algorithm
-- Includes the first point and goes through the second to infinity.
bla :: (Integer, Integer) -> (Integer, Integer) -> [(Integer, Integer)]
bla (x0, y0) (x1, y1) =
  let (dx, dy) = (x1 - x0, y1 - y0)
      xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
      yxStep b (x, y) = (x + signum dx * b, y + signum dy)
      (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                   | otherwise       = (abs dx, abs dy, yxStep)
      walk w xy = xy : walk (tail w) (step (head w) xy)
  in  walk (balancedWord p q 0) (x0, y0)

-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
balancedWord :: Integer -> Integer -> Integer -> [Integer]
balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)
