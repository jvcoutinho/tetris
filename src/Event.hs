module Event (handleInput) where

import qualified State as S
import Graphics.Gloss.Interface.IO.Interact 

handleInput :: Event -> S.State -> S.State
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state  = S.moveCurrentBlock S.Left state 
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = S.moveCurrentBlock S.Right state 
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state  = S.moveCurrentBlock S.Down state
handleInput (EventKey (Char 'a') Down _ _) state            = S.rotateCurrentBlock S.CClockwise state
handleInput (EventKey (Char 'd') Down _ _) state            = S.rotateCurrentBlock S.Clockwise state
handleInput (EventKey (Char 'r') Down _ _) state
    | S.progress state == S.GameOver                        = (S.initialState (S.randomGenerator state))
    | otherwise                                             = state    
handleInput _ state                                         = state 