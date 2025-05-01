module Backend where

import Types
import Randomness

spawnInterval :: Float -- this is shit for spawning a zerg every set period of time.
spawnInterval = 2.0

-- update :: Float -> State -> State
-- update t s = spawnZerg s

-- update :: Float -> State -> State
-- update dt s@(MkState active spawnables time)
--     | time + dt >= spawnInterval =
--         let newState = spawnZerg s
--         in newState { 
--             timeSinceLastSpawn = 0 }
--     | otherwise = s { timeSinceLastSpawn = time + dt }

update :: Float -> State -> State
update dt s@(MkState active spawnables time)
    | time + dt >= spawnInterval =
        let newState = spawnZerg s
        in newState {
            timeSinceLastSpawn = 0,
            activeZergs = moveZergs dt (activeZergs newState)
        }
    | otherwise = s {
        timeSinceLastSpawn = time + dt,
        activeZergs = moveZergs dt active
    }

moveZergs :: Float -> [Zerg] -> [Zerg]
moveZergs dt = map (moveZerg dt)

moveZerg :: Float -> Zerg -> Zerg
moveZerg dt (MkZerg hp speed (x, y)) =
    let dx = -x -- points from zerg's position to center
        dy = -y
        -- normalize direction, needed for consistent speed
        len = sqrt (dx*dx + dy*dy) -- find length
        ux = dx / len              -- find unit vector by dividing by lenght
        uy = dy / len
        moveDist = speed * dt * fpsFloat fps -- scale by speed and time
        newX = x + ux * moveDist -- update position
        newY = y + uy * moveDist
    in MkZerg hp speed (newX, newY)

-- moveDist in moveZerg requires a float for the fps, since fps is an int this is required
-- this insures we also use the global fps value and if it is ever changed this behavior remains the same
fpsFloat :: Int -> Float
fpsFloat = fromIntegral

-- this should have most of the code to move the zergs.