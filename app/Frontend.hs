module Frontend where

import Brillo
import Brillo.Data.Picture

import Types

-- draw :: State -> Picture
-- draw s = Blank

draw :: State -> Picture
draw s = Pictures
     [ Color black (rectangleSolid 100 150) 
     , (Color red (circleSolid 20))]

-- draw :: State -> Picture
-- draw _ = Color black (rectangleSolid 100 150)
-- draw (State x) = Pictures
--      [ Color black (rectangleSolid 100 150)        -- tower at (0, 0)
--      , Translate x 0 (Color red (circleSolid 20))  -- Circle moves toward 0
--      ]

--

-- [ Color black (rectangleSolid 100 150)        -- tower at (0, 0)
--     , Translate x 0 (Color red (circleSolid 20))  -- Circle moves toward 0
--     ]


-- this shoulld do the drawing, from what I understand it should be given the list of active zergs and the tower(s) to draw.