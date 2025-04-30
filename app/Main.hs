module Main where

import Brillo
import Brillo.Data.ViewPort (ViewPort)

window :: Display
window = InWindow "Window" (1024, 768) (10, 10)

bg :: Color
bg = white

fps :: Int
fps = 60

data State = State Float deriving (Eq, Show)

initState :: State
initState = State 400


draw :: State -> Picture
draw (State x) = Pictures
    [ Color black (rectangleSolid 100 150)        -- tower at (0, 0)
    , Translate x 0 (Color red (circleSolid 20))  -- Circle moves toward 0
    ]

-- Moves the circle
update :: ViewPort -> Float -> State -> State
update _ _ (State x)
    | x > 0     = State (x - 1)
    | otherwise = State x

main :: IO ()
main =
    simulate window bg fps
        initState
        draw
        update
        