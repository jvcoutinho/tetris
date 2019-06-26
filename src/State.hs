module State (numCellsWidth, numCellsHeight, initialState, chooseShape, lvl7State, State(..), Board, Progress(..)) where

import Block
import System.Random
import qualified Data.Map.Strict as Map

import Control.Concurrent
import Control.Concurrent.STM

-- The board is a map of coordinates to tetriminos. If there's none, it's an empty cell.
type Board = Map.Map Coordinate (Maybe Tetrimino)

data Progress = Running | GameOver | Paused deriving (Eq)

numCellsWidth, numCellsHeight :: Int
numCellsWidth = 10
numCellsHeight = 28

data State = State {
    randomGenerator :: StdGen,
    level           :: Int,
    currentBlock    :: Block,
    nextShape       :: Tetrimino,
    period          :: Float,
    score           :: Int,
    currentTime     :: Float,
    previousTime    :: Float,
    board           :: Board,
    progress        :: Progress,

    lvl7NextShape   :: IO (MVar Tetrimino)
}

initialState :: StdGen -> State
initialState gen = State {
    randomGenerator = (randomize . randomize) gen,
    level           = 1,
    currentBlock    = (newBlock . fst . chooseShape) gen,
    nextShape       = (fst . chooseShape) (randomize gen),
    period          = 0.5,
    score           = 0,
    currentTime     = 0.0,
    previousTime    = 0.0,
    board           = Map.fromList [ ((x, y), Nothing) | x <- [1..numCellsWidth], y <- [1..numCellsHeight+2] ],
    progress        = Running,

    lvl7NextShape   = newEmptyMVar
}

lvl7State :: MVar Tetrimino -> IO()
lvl7State nextShape = do
    number <- getStdRandom (randomR (1,7))
    putMVar nextShape (tetrimino number)
    threadDelay 2000000
    lvl7State nextShape

randomize :: StdGen -> StdGen
randomize gen = snd (chooseShape gen)

chooseShape :: StdGen -> (Tetrimino, StdGen)
chooseShape gen = (\(tetr, g) -> (tetrimino tetr, g)) (randomR (1,7) gen)