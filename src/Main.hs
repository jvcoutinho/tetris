module Main where

import Block
import Graphics.Gloss

main :: IO ()
main = display FullScreen (makeColorI 39 41 44 255) (text (show (Block J [])))
