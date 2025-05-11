module Main where

import Brillo

import Types ( State(..), towersList, ws, bg, fps, loadTower ) 
import Frontend
import Backend
import Events
import System.Random
import Randomness

window :: Display
window = InWindow "Zerg Rush" (ws, ws) (10, 10)

initState :: RandomGen g => g -> State
initState g =
    let zergs = genStartingPositions 1 g
    in MkState
        { activeZergs = [],
          spawnableZergs = zergs,
          timeSinceLastSpawn = 0, 
          activeTowers = towersList,
          towerImages = undefined, -- To disable a warning
          kills = 0,
          isGameOver = False
        }

-- loads .bmps
initLoader :: StdGen -> [Picture] -> State
initLoader g tImgs = (initState g) { towerImages = tImgs }

main :: IO ()
main = do
    tImgs <- loadTower
    let g = mkStdGen 42
    let initial = initLoader g tImgs
    play window bg fps initial draw handleEvent update
