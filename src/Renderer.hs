module Renderer where

import State
import qualified Data.Map.Strict as Map

import Graphics.Gloss

-- Dimensions.
cellSize, boardWidth, boardHeight, padding :: Int
cellSize = 25
boardWidth = numCellsWidth * cellSize
boardHeight = numCellsHeight * cellSize
padding = 100

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
render state = pictures[renderBoard (board state), renderScore (score state), renderNextShape (nextShape state), renderProgress (progress state)] where

    renderBoard :: Board -> Picture
    renderBoard b = pictures[renderBorder, renderCells visibleBoard] where

        visibleBoard :: Board
        visibleBoard = Map.filterWithKey (\(x, y) _ -> y <= numCellsHeight) b
    
    renderBorder :: Picture
    renderBorder = color borderColor (rectangleSolid tx ty) where
        tx = fromIntegral (boardWidth + padding)
        ty = fromIntegral (boardHeight + padding)

    renderCells :: Board -> Picture
    renderCells b = (translate (-tx) (-ty)) (pictures (Map.elems (Map.mapWithKey renderCell b))) where
        tx = fromIntegral (boardWidth + padding) / 2.5
        ty = fromIntegral (boardHeight + padding) / 2.2

    renderCell :: Coordinate -> Maybe Tetrimino -> Picture
    renderCell (x, y) tetr = translate sx sy (color (cellColor tetr) (rectangleSolid sz sz)) where
        (sx, sy) = transformToScreen (x, y)
        sz = fromIntegral cellSize
        
    transformToScreen :: Coordinate -> (Float, Float)
    transformToScreen (px, py) = (fromIntegral (px * cellSize), fromIntegral (py * cellSize))

    renderScore :: Int -> Picture
    renderScore score = color white (translate 300 300 (scale 0.3 0.3 (text ("SCORE: " ++ show score))))

    renderNextShape :: Tetrimino -> Picture
    renderNextShape shape = translate 300 50 (pictures[nextShapeText, nextShapeFigure (relativeCells shape)]) where

        nextShapeText :: Picture
        nextShapeText = translate 0 50 (color white (scale 0.2 0.2 (text "NEXT:")))

        nextShapeFigure :: [Coordinate] -> Picture
        nextShapeFigure coords = pictures (map toPicture coords) where
            
            toPicture :: Coordinate -> Picture
            toPicture (x, y) = color (cellColor (Just shape)) (translate (-sx) sy (rectangleSolid sz sz)) where
                (sx, sy) = transformToScreen (x, y)
                sz = fromIntegral cellSize

    renderProgress :: Progress -> Picture
    renderProgress Running  = text ""
    renderProgress GameOver = color white (translate 200 (-200) (scale 0.5 0.5 (text "GAME OVER!")))