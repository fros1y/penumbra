module State where


import           Control.Category
import           Control.Lens
import           Prelude              hiding (Either (..), id, (.))

import           Control.Monad        (unless)
import qualified Control.Monad.Random as Random
import qualified Control.Monad.State  as S
import qualified Data.Aeson           as Aeson
import           Data.Default
import           Data.Map.Strict      as Map

import           Coord
import           Serialize
import           Types
import           UISFML
-- import Entity
-- import Level

saveState :: World -> IO ()
saveState w = do
  let filename = "out.penumbra"
  writeFile filename $ show $ Aeson.encode w

loadState :: IO (Maybe World)
loadState = do
  let filename = "out.penumbra"
  fileContents <- readFile filename
  return $ Aeson.decode (read fileContents)
