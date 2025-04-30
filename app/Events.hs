module Events where

import Brillo.Interface.IO.Interact

import Types

handleEvent :: Event -> State -> State
handleEvent e s = s


-- this should handle what happens when the mouse is clicked