module Events where

import Brillo.Data.Point
import Brillo.Interface.IO.Interact

import Types

clickRadius :: Float
clickRadius = 30 -- adjust as needed to match zerg size

isAlive :: Zerg -> Bool
isAlive z = zergHealth z > 0

-- check if a point is within radius of another
isClickHit :: Point -> Point -> Bool
isClickHit (x1, y1) (x2, y2) =
    let dx = x1 - x2
        dy = y1 - y2
    in sqrt (dx*dx + dy*dy) <= clickRadius

-- damage a zerg if it was hit
applyClickdamage :: Point -> Zerg -> Zerg
applyClickdamage clickPos z =
    if isClickHit clickPos (zergPos z)
        then z { zergHealth = zergHealth z - 1 }
        else z

handleEvent :: Event -> State -> State
handleEvent (EventKey (MouseButton LeftButton) Down _ clickPos) s =
--handleEvent (MouseButton pos) s =
    let damaged = map (applyClickdamage clickPos) (activeZergs s)
        survivors = filter isAlive damaged
    in s { activeZergs = survivors }
handleEvent e s = s


-- this should handle what happens when the mouse is clicked