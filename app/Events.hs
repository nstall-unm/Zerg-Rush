module Events where

import Brillo.Data.Point
import Brillo.Interface.IO.Interact

import Types

isAlive :: Zerg -> Bool
isAlive z = zergHealth z > 0

-- Checks if a point is within radius of another
isClickHit :: Point -> Point -> Bool
isClickHit (x1, y1) (x2, y2) =
    let dx = x1 - x2
        dy = y1 - y2
    in sqrt (dx*dx + dy*dy) <= zergRadius + 5 -- Click radius is zerg radius + 5 to make it a little easier

-- Damages a zerg if it was hit
applyClickdamage :: Point -> Zerg -> Zerg
applyClickdamage clickPos z =
    if isClickHit clickPos (zergPos z)
        then z { zergHealth = zergHealth z - 1 }
        else z

handleEvent :: Event -> State -> State
handleEvent (EventKey (MouseButton LeftButton) Down _ clickPos) s =
    let damaged = map (applyClickdamage clickPos) (activeZergs s) -- Maps the zergs that have been clicked and filter dead ones
        survivors = filter isAlive damaged
        clickKills = length (activeZergs s) - length survivors  -- Zergs killed by click
    in s { 
        activeZergs = survivors,
        kills = kills s + clickKills  -- Adds clicked kills to total
    }
handleEvent e s = s
