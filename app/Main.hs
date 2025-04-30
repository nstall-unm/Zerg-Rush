module Main where

import Brillo

window :: Display
window = InWindow "Window" (1024, 768) (10, 10)
bg :: Color
bg = white
fps :: Int
fps = 1

data State = State deriving (Eq, Show)

initState :: State
initState = State

draw :: State -> Picture
draw _ = Color black (rectangleSolid 100 150)

main :: IO ()
main =
    simulate window bg fps
        initState
        draw
        (\_ _ s -> s)
