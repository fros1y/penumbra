{-# LANGUAGE DeriveGeneric #-}
module Direction where

import           GHC.Generics

-- Direction
data Direction = Up | Down | Left | Right deriving (Show, Read, Eq, Generic)
