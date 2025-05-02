module Frontend where

import Brillo
import Brillo.Data.Picture

import Types
import Randomness

-- draw :: State -> Picture
-- draw s = Blank

-- draw :: State -> Picture
-- draw s = Pictures
--      [ Color black (rectangleSolid 100 150) ]


draw :: State -> Picture
draw s = Pictures $
    Color black (rectangleSolid 100 150) : map drawZerg (activeZergs s) -- Add a tuple to call drawTower 

-- Frontend.hs
drawTower :: Tower -> Picture
drawTower (MkTower (x, y) health (w, h)) = Pictures [
    Translate x y $ Color (towerColor health) (rectangleSolid w h),
    Translate (x - 40) (y + h/2 + 30) $  -- Higher Y-position
      Scale 0.3 0.3 $  -- Larger text
      Color white $  -- Contrast color
      text ("HP: " ++ show health)
  ]
  where
    towerColor hp
        | hp > 7 = black
        | hp > 3 = dark red
        | otherwise = red

drawZerg :: Zerg -> Picture
drawZerg (MkZerg hp _ (x, y)) =
  Pictures [Translate x y (Color (healthColor hp) (circleSolid 10))]
  where
    healthColor h = case h of
      3 -> green
      2 -> yellow
      _ -> red

{-
drawZerg :: Zerg -> Picture
drawZerg (MkZerg _ _ (x, y)) =
    Translate x y $
        Color red (circleSolid 10)
-}
-- draw :: State -> Picture
-- draw _ = Color black (rectangleSolid 100 150)
-- draw (State x) = Pictures
--      [ Color black (rectangleSolid 100 150)        -- tower at (0, 0)
--      , Translate x 0 (Color red (circleSolid 20))  -- Circle moves toward 0
--      ]

-- this shoulld do the drawing, from what I understand it should be given the list of active zergs and the tower(s) to draw.