{-# LANGUAGE DeriveGeneric #-}

module Actions where

import           GHC.Generics

import           Coord
import           Entity


-- Action
data Action = ActWait |
              ActMoveBy DeltaCoord |
              ActAttack TargetEntityRef
              deriving (Show, Generic, Eq)

type Actions = [Action]

type EntityActions = EntityRefMap Actions
