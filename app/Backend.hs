module Backend where

import Types
import Brillo.Data.Point
import Data.List (minimumBy)

spawnInterval :: Float -- This is for spawning a zerg for every set period of time.
spawnInterval = 2.0

spawnInterval2 :: Int -> Float
spawnInterval2 killCount = max 0.5 (2.0 - 0.5 * fromIntegral (min 3 (killCount `div` 15)))

update :: Float -> State -> State
update _ s | isGameOver s = s  -- No updates after the game's over
update dt s =
    let movedZergs = moveZergs dt (activeTowers s) (activeZergs s)
        (remainingZergs, updatedTowers) = checkTowerCollision movedZergs (activeTowers s)
        gameIsOver = null updatedTowers
        baseUpdatedState = s {
            timeSinceLastSpawn = timeSinceLastSpawn s + dt,
            activeZergs = remainingZergs,
            activeTowers = updatedTowers,
            kills = kills s,  -- Adds tower kills to total
            isGameOver = gameIsOver
        }
    in if timeSinceLastSpawn s + dt >= spawnInterval2 (kills s)
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

zergCollidesWithTower :: Zerg -> Tower -> Bool
zergCollidesWithTower (MkZerg _ _ _ (zx, zy)) (MkTower (tx, ty) _ (tw, th)) =
    let halfW = tw / 2 + zergRadius
        halfH = th / 2 + zergRadius
    -- Checks if zerg is within the tower's victinity 
    in (zx >= tx - halfW && zx <= tx + halfW) &&
       (zy >= ty - halfH && zy <= ty + halfH)

applyTowerDamage :: Tower -> Int -> Tower
applyTowerDamage (MkTower pos health size) damage =
    MkTower pos (max 0 (health - damage)) size

moveZergs :: Float -> [Tower] -> [Zerg] -> [Zerg]
moveZergs dt tl = map move
  where
    move z
      | zergID z `mod` 5 == 0 = moveZerg arcZerg   dt tl z      -- Spawns an arc zerg every 5th zerg
      | otherwise             = moveZerg defaultMove dt tl z    -- Normal zerg

type Position = (Float, Float)
type DirectionFunc = Float -> Zerg -> [Tower] -> (Float, Float)  -- dt -> zerg -> towers -> movement direction

-- General zerg movement needed for all move functions
moveZerg :: DirectionFunc -> Float -> [Tower] -> Zerg -> Zerg
moveZerg dirFunc dt [] z = z
moveZerg dirFunc dt towers z@(MkZerg zID hp speed (x, y)) =
    let (dx, dy) = dirFunc dt z towers
        -- Normalizes direction, which is needed for consistent speed
        len = sqrt (dx * dx + dy * dy)  -- Finds length
        ux = dx / len                   -- Finds unit vector by dividing length
        uy = dy / len
        moveDist = speed * dt * fpsFloat fps  -- Scales by speed and time
        newX = x + ux * moveDist  -- Updates position
        newY = y + uy * moveDist
    in MkZerg zID hp speed (newX, newY)

-- Moves in a straight line
defaultMove :: DirectionFunc
defaultMove _ (MkZerg _ _ _ (x, y)) towers =
    let (tx, ty) = closestTowerPos (x, y) towers
    in (tx - x, ty - y)

-- Arcing movement
arcZerg :: DirectionFunc
arcZerg dt (MkZerg zID _ _ (x, y)) towers =
    let (tx, ty) = closestTowerPos (x, y) towers
        dx = tx - x
        dy = ty - y
        len = sqrt (dx * dx + dy * dy)
        ux = dx / len
        uy = dy / len
        -- Perpendicular vector (for arc offset)
        perpX = -uy
        perpY = ux
        arcSize = 10 -- Changes how wide the arc is
        arcOffset = sin (dt * 10 + fromIntegral zID) * arcSize
        arcX = ux + perpX * arcOffset
        arcY = uy + perpY * arcOffset
    in (arcX, arcY)

-- moveDist in moveZerg which requires a float for the fps, since fps is an int
-- This insures we also use the global fps value and if it is ever changed, the behavior remains the same
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