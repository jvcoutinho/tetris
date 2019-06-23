module Renderer where

import State
import qualified Data.Map.Strict as Map

import Graphics.Gloss

-- Dimensions.
cellSize, boardWidth, boardHeight, padding :: Int
cellSize = 35
boardWidth = numCellsWidth * cellSize
boardHeight = numCellsHeight * cellSize
padding = (768 - (20 * cellSize)) `quot` 2

-- Colors.
boardColor, borderColor :: Color
boardColor = makeColorI 39 41 44 255
borderColor = makeColorI 146 175 197 255

cellColor :: Maybe Tetrimino -> Color
cellColor Nothing  = boardColor
cellColor (Just I) = dark cyan
cellColor (Just O) = dark yellow
cellColor (Just S) = dark green
cellColor (Just Z) = dark red
cellColor (Just T) = dark magenta
cellColor (Just J) = dark blue
cellColor (Just L) = dark orange

render :: State -> Picture
render state = pictures[renderBoard (board state)] where

    renderBoard :: Board -> Picture
    renderBoard b = pictures[renderBorder, renderCells b]
    
    renderBorder :: Picture
    renderBorder = color borderColor (rectangleSolid tx ty) where
        tx = fromIntegral (boardWidth + padding)
        ty = fromIntegral (boardHeight + padding)

    renderCells :: Board -> Picture
    renderCells b = translate (-tx) (-ty) (pictures (Map.elems (Map.mapWithKey renderCell b))) where
        tx = fromIntegral (quot boardWidth 4)
        ty = fromIntegral (quot boardHeight 4)

    renderCell :: Coordinate -> Maybe Tetrimino -> Picture
    renderCell (x, y) tetr = translate (fromIntegral sx) (fromIntegral sy) (color (cellColor tetr) (rectangleSolid sz sz)) where
        (sx, sy) = transformToScreen (x, y)
        sz = fromIntegral cellSize
        
    transformToScreen :: Coordinate -> Coordinate
    transformToScreen (px, py) = (px * cellSize `quot` 2, py * cellSize `quot` 2)