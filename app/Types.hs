module Types where

import Brillo.Data.Point
import Brillo.Data.Color
import Codec.Picture 
import Brillo (Picture)
 

data Zerg = 
    MkZerg {
        zergHealth :: Int,
        zergSpeed :: Float,
        zergPos :: Point
    } deriving (Eq, Show)

zergStartingHealth :: Int
zergStartingHealth = 3

data Tower = MkTower {
    towerPos :: Point,
    towerHealth :: Int,
    towerSize :: (Float, Float) -- widith and height
} deriving (Eq, Show)

data State =
    MkState {
        activeZergs :: [Zerg],
        spawnableZergs :: [Zerg],
        timeSinceLastSpawn :: Float,
        activeTowers :: [Tower],  -- Changed from gameTower
        kills :: Int,
        isGameOver :: Bool
    } deriving (Eq, Show)

startingTower :: Tower
startingTower = MkTower (60, 0) 10 (100.0, 150.0) -- 10 hp tower in the middle

startingTower2 :: Tower
startingTower2 = MkTower (-60, 0) 10 (100.0, 150.0)

towersList :: [Tower]
towersList = [startingTower, startingTower2]

dark' :: Color -> Color
dark' c = dim (dim c)  -- Makes color darker

-- Window Size
ws :: Int
ws = 1024

-- Background Color
bg :: Color
bg = white

-- Frames per second, tied to game speed as well
fps :: Int
fps = 60