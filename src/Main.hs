module Main where

import Renderer
import State
import Event
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = play FullScreen boardColor 60 initialState render handleInput update
