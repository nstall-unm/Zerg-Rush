module Frontend where

import Brillo
import Types

draw :: State -> Picture
draw s = Pictures $
    map (drawTower (towerImages s)) (activeTowers s) ++
    map (drawZerg (zergImages s)) (activeZergs s) ++
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

drawZerg :: [Picture] -> Zerg -> Picture
drawZerg imgs (MkZerg _ hp _ (x, y)) =
  Translate x y (zergImage hp)
  where
    zergImage h = case h of
      3 -> imgs !! 0  -- zergGREEN.bmp
      2 -> imgs !! 1  -- zergYELLOW.bmp
      _ -> imgs !! 2  -- zergRED.bmp

drawHUD :: State -> Picture
drawHUD s =
  Translate (-500) 400 $  -- Makes sure that it's top-left
    Scale 0.5 0.5 $ -- Larger text
      Color black $ -- Contrast with background
        text ("Kills: " ++ show (kills s))