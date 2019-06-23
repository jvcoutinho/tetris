module State (numCellsWidth, numCellsHeight, initialState, State(..), Tetrimino(..), Coordinate, Board) where

import Block
import System.Random
import qualified Data.Map.Strict as Map

-- The board is a map of coordinates to tetriminos. If there's none, it's an empty cell.
type Board = Map.Map Coordinate (Maybe Tetrimino)

numCellsWidth, numCellsHeight :: Int
numCellsWidth = 10
numCellsHeight = 28

data State = State {
    level :: Int,
    currentBlock :: Block,
    nextShape :: Tetrimino,
    score :: Int,
    randomSeed :: StdGen,
    board :: Board
}

initialState :: State
initialState = State {
    level = 1,
    currentBlock = (newBlock L),
    nextShape = O,
    score = 0,
    randomSeed = mkStdGen 0,
    board = Map.fromList [ ((x, y), Nothing) | x <- [1..numCellsWidth], y <- [1..numCellsHeight] ]
}