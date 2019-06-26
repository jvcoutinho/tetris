module Event (handleInput) where

import State
import qualified Block as B
import Updater
import Graphics.Gloss.Interface.IO.Interact 
import System.Exit

handleInput :: Event -> State -> IO State
handleInput (EventKey (SpecialKey KeyEsc) Down _ _) _       = exitSuccess
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state  = return (moveCurrentBlock B.Left state)
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = return (moveCurrentBlock B.Right state) 
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state  = return (moveCurrentBlock B.Down state)
handleInput (EventKey (Char 'a') Down _ _) state            = return (rotateCurrentBlock B.CClockwise state)
handleInput (EventKey (Char 'd') Down _ _) state            = return (rotateCurrentBlock B.Clockwise state)
handleInput (EventKey (Char 'r') Down _ _) state
    | progress state == GameOver                            = return (initialState (randomGenerator state))
    | otherwise                                             = return state
handleInput _ state                                         = return state 