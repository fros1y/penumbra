{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Symbols where
import           Control.Category
import           Control.Lens
import           Data.Colour       as Colour
import           Data.Colour.Names as Colour
import           Data.Default
import           GHC.Generics
import           Prelude           hiding (Either (..), id, (.))
import qualified SFML.Graphics     as SFML
import qualified SFML.Window       as SFML

-- Symbol
data Symbol = Symbol {
  _glyph          :: Char,
  _baseColor      :: Colour.Colour Double,
  _changeOverTime :: Maybe (SFML.Time -> Symbol)
} deriving (Generic)
makeLenses ''Symbol

instance Default Symbol where
  def = Symbol '?' Colour.white Nothing


flicker :: SFML.Time -> Symbol
flicker t = Symbol '◯' color Nothing where
  one = Colour.yellow
  two = Colour.red
  blend = abs ( sin (fromIntegral (SFML.asMilliseconds t)  / 1000) )
  color = Colour.blend blend one two
