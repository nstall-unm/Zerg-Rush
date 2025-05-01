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
    Color black (rectangleSolid 100 150) : map drawZerg (activeZergs s)

drawZerg :: Zerg -> Picture
drawZerg (MkZerg _ _ (x, y)) =
    Translate x y $
        Color red (circleSolid 10)

-- draw :: State -> Picture
-- draw _ = Color black (rectangleSolid 100 150)
-- draw (State x) = Pictures
--      [ Color black (rectangleSolid 100 150)        -- tower at (0, 0)
--      , Translate x 0 (Color red (circleSolid 20))  -- Circle moves toward 0
--      ]

-- this shoulld do the drawing, from what I understand it should be given the list of active zergs and the tower(s) to draw.