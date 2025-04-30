module Backend where

import Types
import Randomness

-- Moves the circle
-- update :: ViewPort -> Float -> State -> State
-- update _ _ (State x)
--     | x > 0     = State (x - 1)
--     | otherwise = State x

update :: Float -> State -> State
update t s = spawnZerg s



-- this should have most of the code to move the zergs.