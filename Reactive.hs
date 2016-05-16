module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Monad (forever)
import Coord

type EventSource a = (AddHandler a, a -> IO ())
addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

main :: IO ()
main = do
  inputSource <- newAddHandler
  network <- compile $ makeGameNetwork inputSource
  actuate network
  forever (getChar >>= fire inputSource)

makeGameNetwork :: EventSource Char -> MomentIO ()
makeGameNetwork inputSource = do
  eInput <- fromAddHandler $ addHandler inputSource
  playerPosition <- accumB (0 :: Int) $ ((+1) <$ eInput)
  ePlayerPositionUpdate <- plainChanges playerPosition
  reactimate $ putStrLn . show <$> ePlayerPositionUpdate

plainChanges :: Behavior a -> MomentIO (Event a)
plainChanges b = do
   (e, handle) <- newEvent
   eb <- changes b
   reactimate' $ (fmap handle) <$> eb
   return e
