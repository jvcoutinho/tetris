module Block (newBlock, translate, rotate, isCoordinate, tetrimino, Block(..), Tetrimino(..), Coordinate, Direction(..)) where

import Prelude hiding (Left, Right)

-- Block shapes.
data Tetrimino = I | O | T | S | Z | J | L deriving (Eq, Enum)
type Coordinate = (Int, Int)

-- A block is composed by its shape, its current position and the coordinates it occupies. 
data Block = Block Tetrimino Coordinate [Coordinate]

newBlock :: Tetrimino -> Block
newBlock t = Block t blockOrigin (map (sumCoordinates blockOrigin) (relativeCells t)) where

    blockOrigin :: Coordinate
    blockOrigin = (6, 20)

    relativeCells :: Tetrimino -> [Coordinate]
    relativeCells I = [(-2, 0), (-1, 0), (0, 0), (1, 0)]
    relativeCells O = [(-1, 0), (-1, -1), (0, 0), (0, -1)]
    relativeCells T = [(-1, 0), (0, -1), (1, 0)]
    relativeCells S = [(-1, -1), (0, -1), (0, 0), (1, 0)]
    relativeCells Z = [(0, -1), (0, -1), (0, 0), (1, -1)]
    relativeCells J = [(-1, 0), (1, 0), (0, 0), (1, -1)]
    relativeCells L = [(-1, 0), (0, 0), (1, 0), (1, 1)]

    sumCoordinates :: Coordinate -> Coordinate -> Coordinate
    sumCoordinates (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Translate.
data Direction = Left | Right | Down | Clockwise | CClockwise deriving (Eq)
translate :: Direction -> Block -> Block
translate dir (Block shape position coords) = Block shape (translateCoordinates dir position) (map (translateCoordinates dir) coords) where

    translateCoordinates :: Direction -> Coordinate -> Coordinate
    translateCoordinates Left (x, y)  = (x - 1, y)
    translateCoordinates Right (x, y) = (x + 1, y)
    translateCoordinates Down (x, y)  = (x, y - 1)

rotate :: Direction -> Block -> Block
rotate _ b = b

isCoordinate :: Block -> Coordinate -> Bool
isCoordinate (Block _ _ coords) c = elem c coords

tetrimino :: Int -> Tetrimino
tetrimino 1 = I
tetrimino 2 = O
tetrimino 3 = T
tetrimino 4 = S
tetrimino 5 = Z
tetrimino 6 = J
tetrimino 7 = L