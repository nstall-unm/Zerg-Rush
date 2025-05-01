module Types where

import Brillo.Data.Point
import Brillo.Data.Color

data Zerg = 
    MkZerg {
        zergHealth :: Int,
        zergSpeed :: Float,
        zergPos :: Point
    } deriving (Eq, Show)

zergStartingHealth :: Int
zergStartingHealth = 3

data State =
    MkState {
        activeZergs :: [Zerg],
        spawnableZergs :: [Zerg],
        timeSinceLastSpawn :: Float  -- in seconds
    } deriving (Eq, Show)

ws :: Int -- windowSize
ws = 1024

bg :: Color
bg = white

fps :: Int
fps = 60