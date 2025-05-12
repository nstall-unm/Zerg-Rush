module Main where

import Brillo

import Types 
import Frontend
import Backend
import Events
import System.Random
import Randomness

window :: Display
window = InWindow "Zerg Rush" (ws, ws) (10, 10)

initState :: RandomGen g => g -> State
initState g =
    let zergs = genStartingPositions 1 g -- Set Zerg ID to 1, if it is 0 this causes special zergs to spawn first
    in MkState
        { activeZergs = [],
          spawnableZergs = zergs,
          timeSinceLastSpawn = 0, 
          zergImages = undefined, -- To disable a warning, handled in initLoader
          activeTowers = towersList,
          towerImages = undefined, -- To disable a warning, handled in initLoader
          kills = 0,
          isGameOver = False
        }

-- loads .bmps
initLoader :: StdGen -> [Picture] -> [Picture] -> State
initLoader g tImgs zImgs = (initState g) 
  { towerImages = tImgs,
    zergImages = zImgs
  }

main :: IO ()
main = do
    tImgs <- loadTower   -- load tower assets
    zImgs <- loadZergImg -- load zerg assets
    g <- newStdGen       -- random seed every time
    let initial = initLoader g tImgs zImgs
    play window bg fps initial draw handleEvent update
