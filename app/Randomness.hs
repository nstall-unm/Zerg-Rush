module Randomness where

import System.Random

import Types

-- so it turns out random in haskell sucks and haskell sort of depends on "any given function will always give the same output when given the same input" and thats
-- not how random works so this basically takes a seed g and gives back g' to be used next time

-- try not to touch this file unless you really need to

-- TODO: add zerg ID system
genZerg :: RandomGen g => g -> (Zerg, g)
genZerg g = 
    let hws = fromIntegral ws / 2
        (side, g') = uniformR (1 :: Int, 4 :: Int) g
        (xy, g'')  = uniformR (-hws, hws) g'
        (speed, g''') = uniformR (1 :: Float, 5 :: Float) g''
        --p = if odd side then (xy, hws) else (hws, xy)
        p = case side of -- old one only spawned on 2 edges now I have a case statement to spawn on all 4
            1 -> (xy, hws)    -- top
            2 -> (xy, -hws)   -- bottom
            3 -> (-hws, xy)   -- left
            _ -> (hws, xy)    -- right (covers 4)
    in (MkZerg  (zergID g''' = zergID g''' + 1) zergStartingHealth speed p, g''')

genStartingPositions :: RandomGen g => g -> ([Zerg], g)
genStartingPositions g =
    let (zerg, g') = genZerg g
        (zergs, g'') = genStartingPositions g'
    in (zerg : zergs, g'')

spawnZerg :: State -> State
spawnZerg s = case spawnableZergs s of
    (z:zs) -> s { 
        activeZergs = z : activeZergs s,
        spawnableZergs = zs,
        kills = kills s  -- Explicitly preserve kills
    }
    [] -> s