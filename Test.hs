{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (Either (..), (.), id)
import Control.Category
import Control.Lens

import qualified Control.Auto as Auto
import qualified Control.Auto.Run as Auto
import qualified Control.Concurrent.Chan as Chan
import Control.Monad.Fix

data Game = Game Int (Int, Int) deriving Show

type Input = String

turnCount :: Monad m => Auto.Auto m () Int
turnCount = proc _ -> do
  count <- Auto.sumFrom 0   -< 1
  id -< count

game :: Monad m => Auto.Auto m Input Game
game = proc input -> do
  turn <- turnCount -< ()
  command <- handleInput -< input
  pos <- playerPos -< command
  id -< Game turn pos

handleInput :: Monad m => Auto.Auto m Input (Int, Int)
handleInput = proc input -> do
  id -< handleInput' input

handleInput' :: String -> (Int, Int)
handleInput' "up" = (0,1)
handleInput' "down" = (0,-1)
handleInput' "left" = (-1, 0)
handleInput' "right" = (1, 0)
handleInput' _ = (0, 0)

playerPos :: Monad m => Auto.Auto m (Int, Int) (Int, Int)
playerPos = proc input -> do
  let (deltaX, deltaY) = input
  x <- Auto.sumFrom 0 -< deltaX
  y <- Auto.sumFrom 0 -< deltaY
  id -< (x, y)

main :: IO ()
main = loop game ""
  where
    loop a input = do
      (g, a') <- Auto.stepAuto a input
      print g
      input' <- getLine
      loop a' input'
