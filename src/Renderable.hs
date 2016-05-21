module Renderable where
import           Control.Category
import           Control.Lens
import           Data.Colour       as Colour
import           Data.Colour.Names as Colour
import           Data.Default
import           Prelude           hiding (Either (..), id, (.))

import           Entity
import           Symbols

class Renderable a where
  getSymbol :: a -> Symbol

instance (Renderable a) => Renderable (Maybe a) where
  getSymbol (Just a) = getSymbol a
  getSymbol _ = def

instance Renderable Entity where
  getSymbol entity = getSymbol (entity ^. entityType)

instance Renderable EntityType where
  getSymbol Player  = Symbol '@' Colour.white
  getSymbol Floor   = Symbol '·' Colour.dimgray
  getSymbol Rat     = Symbol 'r' Colour.brown
  getSymbol Wall    = Symbol '#' Colour.white
  getSymbol _       = Symbol '?' Colour.dimgray
