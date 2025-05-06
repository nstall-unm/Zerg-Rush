module Backend where

import Types
import Brillo.Data.Point
import Data.List (minimumBy)
import Brillo (arc)


spawnInterval :: Float -- this is shit for spawning a zerg every set period of time.
spawnInterval = 1.0

update :: Float -> State -> State
update dt s =
    --let movedZergs = moveZergs dt (kills s) (activeTowers s) (activeZergs s)
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

zergCollidesWithTower (MkZerg _ _ _ (zx, zy)) (MkTower (tx, ty) _ (tw, th)) =
    let halfW = tw / 2
        halfH = th / 2
    -- Check if zerg is within the tower's victinity 
    in (zx >= tx - halfW && zx <= tx + halfW) &&
       (zy >= ty - halfH && zy <= ty + halfH)

applyTowerDamage :: Tower -> Int -> Tower
applyTowerDamage (MkTower pos health size) damage =
    MkTower pos (max 0 (health - damage)) size

-- TODO: fix kills in this function (this is not a very important issue but it would be nice)
--moveZergs :: Float -> State -> [Tower] -> [Zerg] -> [Zerg]
moveZergs :: Float -> [Tower] -> [Zerg] -> [Zerg]
moveZergs dt tl = map move
  where
    move z
    -- TODO: Add more types of Zerg? Get at least one working first
      | zergID z `mod` 5 == 0 = arcZerg    dt tl z
      | otherwise             = moveZerg   dt tl z
    --   | zergID z `mod` 5 == 0 && kills s >= 10 = specialZerg dt tl z
    --   | otherwise                              = moveZerg    dt tl z

moveZerg :: Float -> [Tower] -> Zerg -> Zerg
moveZerg dt [] z = z  -- no towers? don't move
moveZerg dt towers (MkZerg zID hp speed (x, y)) =
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
    in MkZerg zID hp speed (newX, newY)

arcZerg :: Float -> [Tower] -> Zerg -> Zerg
arcZerg dt [] z = z  -- no towers? don't move
arcZerg dt towers (MkZerg zID hp speed (x, y)) =
    let (tx, ty) = closestTowerPos (x, y) towers
        dx = tx - x
        dy = ty - y
        -- normalize direction, needed for consistent speed
        len = sqrt (dx*dx + dy*dy) -- find length
        ux = dx / len              -- find unit vector by dividing by lenght
        uy = dy / len
        -- perpendicular vector (for arc offset)
        perpX = -uy 
        perpY = ux
        arcSize = 10 -- change how wide the arc is
        arcOffset = sin (dt * 10 + fromIntegral zID) * arcSize
        arcX = ux + perpX * arcOffset
        arcY = uy + perpY * arcOffset
        -- re-normalize vector to account for arc
        arcLen = sqrt (arcX * arcX + arcY * arcY)
        finalUX = arcX / arcLen
        finalUY = arcY / arcLen
        moveDist = speed * dt * fpsFloat fps -- scale by speed and time
        newX = x + finalUX * moveDist -- update position
        newY = y + finalUY * moveDist
    in MkZerg zID hp speed (newX, newY)

-- moveDist in moveZerg requires a float for the fps, since fps is an int this is required
-- this insures we also use the global fps value and if it is ever changed this behavior remains the same
fpsFloat :: Int -> Float
fpsFloat = fromIntegral

closestTowerPos :: Point -> [Tower] -> Point
closestTowerPos p towers = towerPos $ minimumBy (compareDist p) towers
  where
    compareDist (x1, y1) t1 t2 =
        let (x2, y2) = towerPos t1
            (x3, y3) = towerPos t2
            d1 = (x1 - x2)^2 + (y1 - y2)^2
            d2 = (x1 - x3)^2 + (y1 - y3)^2
        in compare d1 d2