module Backend where

import Types
import Randomness

spawnInterval :: Float -- this is shit for spawning a zerg every set period of time.
spawnInterval = 2.0


update :: Float -> State -> State
update dt s =
    let movedZergs = moveZergs dt (activeZergs s)
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


applyTowerDamage :: Tower -> Int -> Tower
applyTowerDamage (MkTower pos health size) damage =
    MkTower pos (max 0 (health - damage)) size


-- TODO: add more movement functions
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