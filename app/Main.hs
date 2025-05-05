module Main where

import Brillo

import Types ( State(..), fps, ws, bg, towersList)  
import Frontend
import Backend
import Events
import System.Random
import Randomness

window :: Display
window = InWindow "Zerg Rush" (ws, ws) (10, 10)

initState :: RandomGen g => g -> State
initState g =
    let (zergs, _) = genStartingPositions g
    in MkState
        { activeZergs = [],
         spawnableZergs = zergs,
         --zergID = 0,
         timeSinceLastSpawn = 0,
         activeTowers = towersList,
         kills = 0,
        isGameOver = False
        }

main :: IO ()
main = do
    let g = mkStdGen 42  -- Constant seed for reproducibility
    play window bg fps (initState g) draw handleEvent update
