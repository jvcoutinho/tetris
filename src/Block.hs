module Block (translate, Block(..), Tetrimino(..), Coordinate) where

import Prelude hiding (Left, Right)

-- Block shapes.
data Tetrimino = I | O | T | S | Z | J | L deriving (Show)
type Coordinate = (Int, Int)

-- A block is composed by its shape and the coordinates it occupies. 
data Block = Block Tetrimino [Coordinate] deriving (Show)

-- Translate.
data Direction = Left | Right | Down
translate :: Direction -> Block -> Block
translate dir (Block shape coords) = Block shape (map (translateCoordinates dir) coords) where

    translateCoordinates :: Direction -> Coordinate -> Coordinate
    translateCoordinates Left (x, y)  = (x - 1, y)
    translateCoordinates Right (x, y) = (x + 1, y)
    translateCoordinates Down (x, y)  = (x, y - 1)