module Frontend where

import Brillo

import Types

draw :: State -> Picture
draw s = Pictures $
    map drawTower (activeTowers s) ++
    map drawZerg (activeZergs s) ++  -- Draw zergs first
    [ drawHUD s ]                    -- Draw HUD on top

drawTower :: Tower -> Picture
drawTower (MkTower (x, y) health (w, h)) = Pictures [
    Translate x y (Color (towerColor health) (rectangleSolid w h)),
    Translate (x - 40) (y + h/2 + 30) (
      Scale 0.2 0.2 (
        Color black (
          text ("HP: " ++ show health)
        )
      )
    )
  ]

  where
    towerColor hp
        | hp > 7 = black
        | hp > 3 = dark red
        | otherwise = red

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