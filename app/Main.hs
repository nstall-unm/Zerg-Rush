module Main where

import Brillo
import Brillo.Data.ViewPort

import Types
import Frontend
import Backend
import Events
import System.Random

import Randomness

window :: Display
window = InWindow "Window" (ws, ws) (10, 10)

initState :: RandomGen g => g -> State
initState g = 
    let (zergs, _) = genStartingPositions g
    in MkState [] zergs 0 startingTower

main :: IO ()
main = do
    g <- initStdGen -- Random seed
    -- let g = mkStdGen 10 -- Constant seed
    -- let (zergs, g') = genStartingPositions g
    -- print (head zergs)
    play window bg fps
        (initState g)
        draw
        handleEvent
        update
