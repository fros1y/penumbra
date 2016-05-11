module ECS where

import qualified Data.IntMap as IntMap
import Data.Maybe (isJust, fromJust)
import qualified Data.List as List
type EntityRef = Int

type Position = (Int, Int)

newtype Components a = Components (IntMap.IntMap a) deriving Show

class ComponentStore a where
  getFor :: Components a -> EntityRef -> Maybe a
  getFor' :: Components a -> EntityRef -> a
  getFor' components ref = fromJust $ getFor components ref
  setFor :: Components a -> EntityRef -> a -> Components a
  deleteFor :: Components a -> EntityRef -> Components a
  existsFor :: Components a -> EntityRef -> Bool
  existsFor components ref = isJust $ getFor components ref
  refsWith :: Components a -> [EntityRef]
  refsWithAll :: [Components a] -> [EntityRef]
  refsWithAll [] = []
  refsWithAll [c] = refsWith c
  refsWithAll (c:cs) = refsWith c `List.intersect` refsWithAll cs

instance ComponentStore (Components a) where
  getFor (Components components) ref = IntMap.lookup ref components
  setFor (Components components) ref a = Components $ IntMap.insert ref a components
  deleteFor (Components components) ref = Components $ IntMap.delete ref components
  refsWith (Components components) = IntMap.keys components
