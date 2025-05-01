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

-- Window Size
ws :: Int
ws = 1024

-- Background Color
bg :: Color
bg = white

-- Frames per second, tied to game speed as well
fps :: Int
fps = 60