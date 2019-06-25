module Main where

import Renderer
import State
import Event

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

main :: IO ()
main = do
    numberGenerator <- newStdGen
    playIO FullScreen boardColor 60 (initialState numberGenerator) render handleInput update
