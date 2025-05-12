module Randomness where

import System.Random

import Types

genZerg :: RandomGen g => Int -> g -> (Zerg, Int, g)
genZerg zID g = 
    let hws = fromIntegral ws / 2
        (side, g') = uniformR (1 :: Int, 4 :: Int) g
        (xy, g'')  = uniformR (-hws, hws) g'
        (speed, g''') = uniformR (1 :: Float, 5 :: Float) g''

        p = case side of -- Case statement to spawn on all 4 zergs
            1 -> (xy, hws)    -- Top
            2 -> (xy, -hws)   -- Bottom
            3 -> (-hws, xy)   -- Left
            _ -> (hws, xy)    -- Right (covers all 4 sides)
        z = MkZerg zID zergStartingHealth speed p
    in (z, zID + 1, g''')

genStartingPositions :: RandomGen g => Int -> g -> [Zerg]
genStartingPositions zID g =
    let (zerg, zID', g') = genZerg zID g
    in zerg : genStartingPositions zID' g'

spawnZerg :: State -> State
spawnZerg s = case spawnableZergs s of
    (z:zs) -> s { 
        activeZergs = z : activeZergs s,
        spawnableZergs = zs,
        kills = kills s  -- Explicitly preserve kills
    }
    [] -> s