module State where

import Block
import System.Random

data State = State {
    level :: Int,
    currentBlock :: Block,
    nextShape :: Tetrimino,
    score :: Int,
    randomSeed :: StdGen
}

initialState :: State
initialState = State {
    level = 1,
    currentBlock = (newBlock L),
    nextShape = O,
    score = 0,
    randomSeed = mkStdGen 0
}