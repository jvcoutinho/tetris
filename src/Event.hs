module Event where

import State
import qualified Graphics.Gloss.Interface.IO.Interact as I

handleInput :: Event -> State -> State
handleInput (EventKey (SpecialKey KeyLeft) I.Down _ _) state  = moveCurrentBlock Left state 
handleInput (EventKey (SpecialKey KeyRight) I.Down _ _) state = moveCurrentBlock Right state 
handleInput (EventKey (SpecialKey KeyDown) I.Down _ _) state  = moveCurrentBlock State.Down state
handleInput (EventKey (Char 'a') I.Down _ _) state            = rotateCurrentBlock CClockwise state
handleInput (EventKey (Char 'd') I.Down _ _) state            = rotateCurrentBlock Clockwise state 