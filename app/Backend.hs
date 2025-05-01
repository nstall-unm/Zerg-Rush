module Backend where

import Types
import Randomness

-- Moves the circle
-- update :: ViewPort -> Float -> State -> State
-- update _ _ (State x)
--     | x > 0     = State (x - 1)
--     | otherwise = State x

spawnInterval :: Float -- this is shit for spawning a zerg every set period of time.
spawnInterval = 2.0

-- update :: Float -> State -> State
-- update t s = spawnZerg s

update :: Float -> State -> State
update dt s@(MkState active spawnables time)
    | time + dt >= spawnInterval =
        let newState = spawnZerg s
        in newState { timeSinceLastSpawn = 0 }
    | otherwise = s { timeSinceLastSpawn = time + dt }



-- this should have most of the code to move the zergs.