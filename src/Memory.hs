{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Memory where
import           GHC.Generics
import Data.Default
import Control.Lens
import Coord
import Entity
import Data.Map as Map

data Memory = Memory {
  _entityLocations :: EntityRefMap Coord
} deriving (Show, Generic)
makeLenses ''Memory

instance Default Memory where
  def = Memory def

data SeenRemembered = Seen | Remembered deriving (Eq, Generic)
