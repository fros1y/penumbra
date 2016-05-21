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
  _baseColor      :: Colour.Colour Double
} deriving (Generic)
makeLenses ''Symbol

instance Show Symbol where
  show Symbol {_glyph = glyph} = show glyph

instance Default Symbol where
  def = Symbol '?' Colour.white
