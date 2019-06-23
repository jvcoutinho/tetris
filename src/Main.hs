module Main where

import Renderer
import State
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display

main :: IO ()
main = display FullScreen boardColor (render initialState)
