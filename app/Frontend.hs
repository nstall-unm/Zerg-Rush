module Frontend where

import Brillo.Data.Picture

import Types

draw :: State -> Picture
draw s = Blank

--

-- [ Color black (rectangleSolid 100 150)        -- tower at (0, 0)
--     , Translate x 0 (Color red (circleSolid 20))  -- Circle moves toward 0
--     ]


-- this shoulld do the drawing, from what I understand it should be given the list of active zergs and the tower(s) to draw.