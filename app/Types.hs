module Types where

import Brillo.Data.Point
import Brillo.Data.Color
import Brillo

data Zerg = 
    MkZerg {
        zergID :: Int,
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
        zergImages :: [Picture],  -- list of zerg assets
        activeTowers :: [Tower], 
        towerImages  :: [Picture], -- list of tower assets
        kills :: Int,
        isGameOver :: Bool
    } deriving (Eq, Show)

-- loads tower sprites
loadTower :: IO [Picture]
loadTower = mapM loadBMP [
    "resources/tower/towerFULL.bmp", -- full health sprite
    "resources/tower/towerDMG.bmp",  -- partial (damgaged) health sprite
    "resources/tower/towerDEST.bmp"  -- low health (destroyed) sprite
  ]

-- loads zerg sprites
loadZergImg :: IO [Picture]
loadZergImg = mapM loadBMP [    
    "resources/zerg/zergGREEN.bmp",
    "resources/zerg/zergYELLOW.bmp",
    "resources/zerg/zergRED.bmp"
  ]

startingTower :: Tower
startingTower = MkTower (200, -100) 10 (100.0, 150.0) -- 10 hp tower in the middle

startingTower2 :: Tower
startingTower2 = MkTower (-200, -100) 10 (100.0, 150.0)

startingTower3 :: Tower
startingTower3 = MkTower (0, 150) 10 (100.0, 150.0)

towersList :: [Tower]
towersList = [startingTower, startingTower2, startingTower3]


zergRadius :: Float
zergRadius = 10 -- size of the zergs used in hit detection and click detection

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