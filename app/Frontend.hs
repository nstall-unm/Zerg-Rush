module Frontend where

import Brillo

import Types

draw :: State -> Picture
draw s = Pictures $
    map (drawTower (towerImages s)) (activeTowers s) ++
    map drawZerg (activeZergs s) ++
    [drawHUD s]
  
drawTower :: [Picture] -> Tower -> Picture
drawTower imgs (MkTower (x, y) health (w, h)) = Pictures [
    Translate x y (towerImage health),
    Translate (x - 40) (y + h/2 + 30) (
      Scale 0.2 0.2 (
        Color black (
          text ("HP: " ++ show health)
        )
      )
    )
  ]
  where
    towerImage hp
        | hp > 7     = imgs !! 0 -- towerFULL.bmp
        | hp > 3     = imgs !! 1 -- towerDMG.bmp
        | otherwise  = imgs !! 2 -- towerDEST.bmp

drawZerg :: Zerg -> Picture
drawZerg (MkZerg _ hp _ (x, y)) =
  Pictures [Translate x y (Color (healthColor hp) (circleSolid 10))]
  where
    healthColor h = case h of
      3 -> green
      2 -> yellow
      _ -> red

drawHUD :: State -> Picture
drawHUD s =
  Translate (-500) 400 $  -- Make sure it's top-left, adjust as needed
    Scale 0.5 0.5 $ -- Larger text
      Color black $ -- Contrast with background
        text ("Kills: " ++ show (kills s))

-- this shoulld do the drawing, from what I understand it should be given the list of active zergs and the tower(s) to draw.