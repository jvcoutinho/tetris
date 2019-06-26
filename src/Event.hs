module Event (handleInput) where

import qualified State as S
import Updater
import Graphics.Gloss.Interface.IO.Interact 
import System.Exit

handleInput :: Event -> S.State -> IO S.State
handleInput (EventKey (SpecialKey KeyEsc) Down _ _) _       = exitSuccess
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state  = return (moveCurrentBlock S.Left state)
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = return (moveCurrentBlock S.Right state) 
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state  = return (moveCurrentBlock S.Down state)
handleInput (EventKey (Char 'a') Down _ _) state            = return (rotateCurrentBlock S.CClockwise state)
handleInput (EventKey (Char 'd') Down _ _) state            = return (rotateCurrentBlock S.Clockwise state)
handleInput (EventKey (Char 'r') Down _ _) state
    | S.progress state == S.GameOver                        = return (S.initialState (S.randomGenerator state))
    | otherwise                                             = return state
handleInput _ state                                         = return state 