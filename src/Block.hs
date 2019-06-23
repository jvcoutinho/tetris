module Block (newBlock, translate, rotate, Block(..), Tetrimino(..), Coordinate, Direction(..)) where

import Prelude hiding (Left, Right)

-- Block shapes.
data Tetrimino = I | O | T | S | Z | J | L
type Coordinate = (Int, Int)

-- A block is composed by its shape and the coordinates it occupies. 
data Block = Block Tetrimino [Coordinate]

newBlock :: Tetrimino -> Block
newBlock I = Block I [(-2, 0), (-1, 0), (1, 0)]
newBlock O = Block O [(-1, 0), (-1, -1), (0, -1)]
newBlock T = Block T [(-1, 0), (0, -1), (1, 0)]
newBlock S = Block S [(-1, -1), (0, -1), (1, 0)]
newBlock Z = Block Z [(0, -1), (0, -1), (1, -1)]
newBlock J = Block J [(-1, 0), (1, 0), (1, -1)]
newBlock L = Block L [(-1, 0), (0, -1), (1, 0)]

-- Translate.
data Direction = Left | Right | Down | Clockwise | CClockwise
translate :: Direction -> Block -> Block
translate dir (Block shape coords) = Block shape (map (translateCoordinates dir) coords) where

    translateCoordinates :: Direction -> Coordinate -> Coordinate
    translateCoordinates Left (x, y)  = (x - 1, y)
    translateCoordinates Right (x, y) = (x + 1, y)
    translateCoordinates Down (x, y)  = (x, y - 1)

rotate :: Direction -> Block -> Block
rotate _ b = b