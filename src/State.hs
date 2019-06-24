module State (numCellsWidth, numCellsHeight, initialState, update, moveCurrentBlock, rotateCurrentBlock, State(..), Tetrimino(..), Direction(..), Coordinate, Board) where

import Block
import System.Random
import qualified Data.Map.Strict as Map

-- The board is a map of coordinates to tetriminos. If there's none, it's an empty cell.
type Board = Map.Map Coordinate (Maybe Tetrimino)

numCellsWidth, numCellsHeight :: Int
numCellsWidth = 10
numCellsHeight = 28

data State = State {
    randomSeed :: StdGen,
    level :: Int,
    currentBlock :: Block,
    nextShape :: Tetrimino,
    period :: Float,
    score :: Int,
    currentTime :: Float,
    previousTime :: Float,
    board :: Board
}

initialState :: State
initialState = State {
    randomSeed = mkStdGen 0,
    level = 1,
    nextShape = L,
    currentBlock = (newBlock T),
    period = 1.0,
    score = 0,
    currentTime = 0.0,
    previousTime = 0.0,
    
    board = Map.fromList [ ((x, y), Nothing) | x <- [1..numCellsWidth], y <- [1..numCellsHeight] ]
}

-- generateNextShape :: StdGen -> Tetrimino
-- generateNextShape gen = fst (randomR (1, 7) gen)

moveCurrentBlock :: Direction -> State -> State
moveCurrentBlock dir state = updateCurrentBlock translate dir state

rotateCurrentBlock :: Direction -> State -> State
rotateCurrentBlock dir state = updateCurrentBlock rotate dir state

update :: Float -> State -> State
update f state = checkIfMove (state {currentTime = f + currentTime state}) where

    checkIfMove :: State -> State
    checkIfMove s 
        | (currentTime s) - (previousTime s) >= (period s) = updateCurrentBlock translate Down (s {previousTime = currentTime s})
        | otherwise                                        = s

updateCurrentBlock :: (Direction -> Block -> Block) -> Direction -> State -> State
updateCurrentBlock f dir s
    | validMovement (currentBlock s) (f dir (currentBlock s)) (board s) = updateBoard (s {currentBlock = f dir (currentBlock s)}) s
    | dir == Down                                                       = checkFullLines (s {currentBlock = newBlock (nextShape s)})
    | otherwise                                                         = s
        where
            validMovement :: Block -> Block -> Board -> Bool
            validMovement _ (Block _ _ []) _             = True
            validMovement block (Block s p (c:cs)) board = (isCoordinate block c || (Map.lookup c board) == Just Nothing) 
                                                                && validMovement block (Block s p cs) board

            checkFullLines :: State -> State
            checkFullLines s = (\(b, c) -> s {board = b, score = updateScore c}) cleantFullLines
                    where
                        cleantFullLines :: (Board, Int)
                        cleantFullLines = clearFullLines numCellsHeight (board s)

                        clearFullLines :: Int -> Board -> (Board, Int)
                        clearFullLines 0 b = (b, 0)
                        clearFullLines n b 
                            | isFull n b = (\(b, c) -> (b, 1 + c)) (clearFullLines (n - 1) (Map.union downLines b))
                            | otherwise  = clearFullLines (n - 1) b
                                where
                                    isFull :: Int -> Board -> Bool
                                    isFull n b = Map.size (Map.filterWithKey (\k v -> snd k == n && v /= Nothing) b) == numCellsWidth
                                    
                                    downLines :: Board
                                    downLines = Map.union (emptyLine numCellsHeight) (Map.mapWithKey decreaseRow b) where

                                        emptyLine :: Int -> Board
                                        emptyLine n = Map.fromList [ ((x, n), Nothing) | x <- [1..numCellsWidth] ]

                                        decreaseRow :: Coordinate -> Maybe Tetrimino -> Maybe Tetrimino
                                        decreaseRow (x, y) tetr 
                                            | y >= n && y < numCellsHeight = b Map.! (x, y + 1)
                                            | otherwise                    = tetr

                        updateScore :: Int -> Int
                        updateScore count = (score s) + count * count * 10

updateBoard :: State -> State -> State
updateBoard newState oldState = newState {board = setBoard True cleantBoard (currentBlock newState)} where

    cleantBoard :: Board
    cleantBoard = setBoard False (board oldState) (currentBlock oldState)

    setBoard :: Bool -> Board -> Block -> Board
    setBoard _ board (Block _ _ [])              = board 
    setBoard set board (Block shape pos (c:cs))  = Map.adjust (\x -> if set then Just shape else Nothing) c (setBoard set board (Block shape pos cs))
