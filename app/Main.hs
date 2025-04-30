module Main where

import Brillo
import System.Random (randomRIO)

window :: Display
window = InWindow "Window" (1024, 768) (10, 10)

bg :: Color
bg = white

fps :: Int
fps = 1

data State = State deriving (Eq, Show)

initState :: State
initState = State

-- Random Y spawnpoint (on left edge)
-- initState :: IO State
-- initState = do
--     y <- 

draw :: State -> Picture
--draw _ = Color green (rectangleSolid 300 250)
draw _ = Pictures   
        [ Translate 0 0 $ Color green (rectangleSolid 300 250)
        , translate (-512) 0 $ Color blue (circleSolid 20)]

main :: IO ()
main =
    simulate window bg fps
        initState
        draw
        (\_ _ s -> s)
