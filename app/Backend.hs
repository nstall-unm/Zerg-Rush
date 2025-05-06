module Backend where

import Types
import Randomness
import Brillo.Data.Point
import Data.List (minimumBy)


spawnInterval :: Float -- this is shit for spawning a zerg every set period of time.
spawnInterval = 1.0


update :: Float -> State -> State
update dt s =
    let movedZergs = moveZergs dt (activeTowers s) (activeZergs s)
        (remainingZergs, updatedTowers) = checkTowerCollision movedZergs (activeTowers s)
        towerKills = length movedZergs - length remainingZergs  -- Zergs destroyed by towers
        baseUpdatedState = s {
            timeSinceLastSpawn = timeSinceLastSpawn s + dt,
            activeZergs = remainingZergs,
            activeTowers = updatedTowers,
            kills = kills s + towerKills  -- Add tower kills to total
            -- = trace ("Tower kills: " ++ show towerKills) () --debug statement 
        }
    in if timeSinceLastSpawn s + dt >= spawnInterval
        then case spawnableZergs baseUpdatedState of
            (z:zs) -> baseUpdatedState {
                activeZergs = z : activeZergs baseUpdatedState,
                spawnableZergs = zs,
                timeSinceLastSpawn = 0
            }
            [] -> baseUpdatedState
        else baseUpdatedState

-- update :: Float -> State -> State
-- update dt s@(MkState active spawnables time tower) =
--     -- Move zergs first
--     let movedZergs = moveZergs dt active
--         -- Check collisions with updated positions
--         (remainingZergs, towerDamage) = checkTowerCollision movedZergs tower
--         --updatedTower = applyTowerDamage tower towerDamage
--         (updatedZergs, updatedTowers) = checkTowerCollision movedZergs (activeTowers s)
--         -- Update time and tower FIRST
--         baseUpdatedState = s {
--             timeSinceLastSpawn = time + dt,
--             activeZergs = remainingZergs,
--             activeTowers = updatedTowers
--             -- gameTower = updatedTower
--         }
--     in if time + dt >= spawnInterval
--         then case spawnableZergs baseUpdatedState of  -- Use UPDATED state
--             (z:zs) -> baseUpdatedState {
--                 activeZergs = z : remainingZergs,  -- Add new zerg
--                 spawnableZergs = zs,
--                 timeSinceLastSpawn = 0
--             }
--             [] -> baseUpdatedState
--         else baseUpdatedState

isTowerAlive :: Tower -> Bool
isTowerAlive t = towerHealth t > 0

checkTowerCollision :: [Zerg] -> [Tower] -> ([Zerg], [Tower])
checkTowerCollision zergs towers =
    let (remainingZergs, damagedTowers) = foldr check ([], towers) zergs
        aliveTowers = filter isTowerAlive damagedTowers
    in (remainingZergs, aliveTowers)
  where
    check z (zsAcc, ts) =
      case collideAndDamage z ts of
        Just updatedTowers -> (zsAcc, updatedTowers)
        Nothing -> (z:zsAcc, ts)

-- checkTowerCollision :: [Zerg] -> [Tower] -> ([Zerg], [Tower])
-- checkTowerCollision zergs towers =
--     let (remainingZergs, damagedTowers) = foldr check ([], towers) zergs
--         aliveTowers = filter isTowerAlive damagedTowers
--     in (remainingTowers, aliveTowers)
--   where
--     check z (zsAcc, ts) = 
--       case collideAndDamage z ts of
--        Just updatedTowers -> (zsAcc, updatedTowers)
--        Nothing -> (z:zsAcc, ts)


-- checkTowerCollision :: [Zerg] -> [Tower] -> ([Zerg], [Tower])
-- checkTowerCollision zergs towers =
--     foldr check ([], towers) zergs
--   where
--     check z (zsAcc, ts) =
--       case collideAndDamage z ts of
--         Just updatedTowers -> (zsAcc, updatedTowers)  -- Zerg hit a tower
--         Nothing -> (z:zsAcc, ts)                      -- Zerg survived
        
-- -- Check collisions between zergs and tower
-- checkTowerCollision :: [Zerg] -> [Tower] -> ([Zerg], Int)
-- checkTowerCollision zergs (MkTower (tx, ty) _ (tw, th)) =
--     foldr (\z (zs, dmg) -> 
--         if zergCollidesWithTower z (tw, th)
--         then (zs, dmg + 1)  -- Count damage, remove zerg
--         else (z:zs, dmg)
--     ) ([], 0) zergs

collideAndDamage :: Zerg -> [Tower] -> Maybe [Tower]
collideAndDamage z [] = Nothing
collideAndDamage z (t:ts)
  | zergCollidesWithTower z t =
      let damagedTower = applyTowerDamage t 1
      in Just (damagedTower : ts)
  | otherwise =
      case collideAndDamage z ts of
        Just rest -> Just (t : rest)
        Nothing -> Nothing

zergCollidesWithTower (MkZerg _ _ (zx, zy)) (MkTower (tx, ty) _ (tw, th)) =
    let halfW = tw / 2
        halfH = th / 2
    -- Check if zerg is within the tower's victinity 
    in (zx >= tx - halfW && zx <= tx + halfW) &&
       (zy >= ty - halfH && zy <= ty + halfH)

-- zergCollidesWithTower :: Zerg -> (Float, Float) -> Bool
-- zergCollidesWithTower (MkZerg _ _ (zx, zy)) (tw, th) =
--     let halfW = tw/2
--         halfH = th/2
--     in abs zx <= halfW && abs zy <= halfH

applyTowerDamage :: Tower -> Int -> Tower
applyTowerDamage (MkTower pos health size) damage =
    MkTower pos (max 0 (health - damage)) size

moveZergs :: Float -> [Tower] -> [Zerg] -> [Zerg]
moveZergs dt towersList = map (moveZerg dt towersList)

moveZerg :: Float -> [Tower] -> Zerg -> Zerg
moveZerg dt [] z = z  -- no towers? don't move
moveZerg dt towers (MkZerg hp speed (x, y)) =
    let (tx, ty) = closestTowerPos (x, y) towers
        dx = tx - x -- points from zerg's position to nearest tower
        dy = ty - y
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

closestTowerPos :: Point -> [Tower] -> Point
closestTowerPos p towers = towerPos $ minimumBy (compareDist p) towers
  where
    compareDist (x1, y1) t1 t2 =
        let (x2, y2) = towerPos t1
            (x3, y3) = towerPos t2
            d1 = (x1 - x2)^2 + (y1 - y2)^2
            d2 = (x1 - x3)^2 + (y1 - y3)^2
        in compare d1 d2

fpsFloat :: Int -> Float
fpsFloat = fromIntegral

-- this should have most of the code to move the zergs.

-- update :: Float -> State -> State
-- update t s = spawnZerg s

-- update :: Float -> State -> State
-- update dt s@(MkState active spawnables time)
--     | time + dt >= spawnInterval =
--         let newState = spawnZerg s
--         in newState { 
--             timeSinceLastSpawn = 0 }
--     | otherwise = s { timeSinceLastSpawn = time + dt }

{-
update :: Float -> State -> State
update dt s@(MkState active spawnables time tower)
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
-}