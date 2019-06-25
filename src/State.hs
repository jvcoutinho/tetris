module State (numCellsWidth, numCellsHeight, initialState, update, moveCurrentBlock, rotateCurrentBlock, relativeCells, State(..), Tetrimino(..), Direction(..), Coordinate, Board, Progress(..)) where

import Block
import System.Random
import qualified Data.Map.Strict as Map

import Control.Concurrent.STM

-- The board is a map of coordinates to tetriminos. If there's none, it's an empty cell.
type Board = Map.Map Coordinate (Maybe Tetrimino)

data Progress = Running | GameOver | Paused deriving (Eq)

numCellsWidth, numCellsHeight :: Int
numCellsWidth = 10
numCellsHeight = 28

nextLevelTime :: Float
nextLevelTime = 40.0

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
    lvl5NextShape   :: STM (TVar Tetrimino)
}

initialState :: StdGen -> State
initialState gen = State {
    randomGenerator = (randomize . randomize) gen,
    level           = 1,  -- TODO: end game and more levels
    currentBlock    = (newBlock . fst . chooseShape) gen,
    nextShape       = (fst . chooseShape) (randomize gen),
    period          = 0.5,
    score           = 0,
    currentTime     = 0.0,
    previousTime    = 0.0,
    board           = Map.fromList [ ((x, y), Nothing) | x <- [1..numCellsWidth], y <- [1..numCellsHeight+2] ],
    progress        = Running,

    lvl5NextShape   = newTVar ((fst . chooseShape) (randomize gen))
}

randomize :: StdGen -> StdGen
randomize gen = snd (chooseShape gen)

chooseShape :: StdGen -> (Tetrimino, StdGen)
chooseShape gen = (\(tetr, g) -> (tetrimino tetr, g)) (randomR (1,7) gen)

moveCurrentBlock :: Direction -> State -> State
moveCurrentBlock dir state = updateCurrentBlock translate dir state

rotateCurrentBlock :: Direction -> State -> State
rotateCurrentBlock dir state = updateCurrentBlock rotate dir state

update :: Float -> State -> IO State
update f state = progression (state {currentTime = f + currentTime state}) where
       
    progression :: State -> IO State
    progression s
        | levelPass    = if level nextLevel == 5 then return nextLevel else return nextLevel
        | periodPass   = return (updateCurrentBlock translate Down (s {previousTime = currentTime s}))
        | otherwise    = return s
        where
            nextLevel :: State
            nextLevel = s {level = (level s) + 1, period = max 0.1 ((period s) - 0.1)}

            levelPass :: Bool
            levelPass = currentTime s  >= fromIntegral (level s) * nextLevelTime

            periodPass :: Bool
            periodPass = (currentTime s) - (previousTime s) >= (period s)

updateCurrentBlock :: (Direction -> Block -> Block) -> Direction -> State -> State
updateCurrentBlock f dir s
    | progress s == GameOver                             = s
    | validMovement (currentBlock s) nextBlock (board s) = updateBoard (s {currentBlock = nextBlock}) s
    | dir == Down                                        = if passedLimits (currentBlock s) (board s) 
                                                            then s {progress = GameOver}
                                                            else checkFullLines (s {nextShape = shape, currentBlock = newBlock (nextShape s), randomGenerator = gen})
    | otherwise                                          = s
        where
            nextBlock :: Block
            nextBlock = f dir (currentBlock s)

            validMovement :: Block -> Block -> Board -> Bool
            validMovement _ (Block _ _ []) _             = True
            validMovement block (Block s p (c:cs)) board = (isCoordinate block c || (Map.lookup c board) == Just Nothing) 
                                                                && validMovement block (Block s p cs) board

            passedLimits :: Block -> Board -> Bool
            passedLimits (Block _ _ []) _             = False
            passedLimits (Block s p ((x,y):cs)) board = y > numCellsHeight || passedLimits (Block s p cs) board

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

            (shape, gen) = chooseShape (randomGenerator s) 

updateBoard :: State -> State -> State
updateBoard newState oldState = newState {board = setBoard True cleantBoard (currentBlock newState)} where

    cleantBoard :: Board
    cleantBoard = setBoard False (board oldState) (currentBlock oldState)

    setBoard :: Bool -> Board -> Block -> Board
    setBoard _ board (Block _ _ [])              = board 
    setBoard set board (Block shape pos (c:cs))  = Map.adjust (\x -> if set then Just shape else Nothing) c (setBoard set board (Block shape pos cs))
