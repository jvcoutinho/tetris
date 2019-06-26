module Updater (update, moveCurrentBlock, rotateCurrentBlock) where
    
import State
import Block
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Control.Concurrent

nextLevelTime = 5.0

moveCurrentBlock :: Direction -> State -> State
moveCurrentBlock dir state = updateCurrentBlock translate dir state

rotateCurrentBlock :: Direction -> State -> State
rotateCurrentBlock dir state = updateCurrentBlock rotate dir state

update :: Float -> State -> IO State
update deltaTime s = progression (s {currentTime = deltaTime + currentTime s})

progression :: State -> IO State
progression s 
    | levelPassed s  = levelUpState s
    | periodPassed s = updateState s
    | otherwise      = return s

levelPassed :: State -> Bool
levelPassed s = level s < 10 && (currentTime s  >= fromIntegral (level s) * nextLevelTime)

periodPassed :: State -> Bool
periodPassed s = (currentTime s) - (previousTime s) >= (period s)

-- When the player reaches level 7, a thread is spawned to randomize next shapes.
levelUpState :: State -> IO State
levelUpState s = do
    let nextLevelState = s {level = (level s) + 1, period = max 0.1 ((period s) - 0.1)}
    if level nextLevelState == 7
        then do
            shape <- lvl7NextShape s
            forkIO (lvl7State shape)
            return nextLevelState
        else
            return nextLevelState 
        
updateState :: State -> IO State
updateState s = do
    shape <- lvl7NextShape s
    possiblyNextShape <- tryTakeMVar shape
    return (updateCurrentBlock translate Down (nextState possiblyNextShape shape))

    where
        nextState shape mVar = s { previousTime = currentTime s, nextShape = fromMaybe (nextShape s) shape, lvl7NextShape = return mVar }

updateCurrentBlock :: (Direction -> Block -> Block) -> Direction -> State -> State
updateCurrentBlock move dir s
    | isGameOver s                                           = s
    | isValidMovement (currentBlock s) resultBlock (board s) = updatedBoardState updatedBlockState s
    | dir == Down                                            = if reachedCeil (currentBlock s) (board s)
                                                                then gameOverState
                                                                else fullLinesErased newBlockState
    | otherwise                                              = s
    
    where
        resultBlock       = move dir (currentBlock s)
        updatedBlockState = s {currentBlock = resultBlock}
        gameOverState     = s {progress = GameOver}
        newBlockState     = s {nextShape = shape, currentBlock = newBlock (nextShape s), randomGenerator = gen}
        (shape, gen)      = chooseShape (randomGenerator s) 

reachedCeil :: Block -> Board -> Bool
reachedCeil block board = foldr (||) False (map passedUpLimit (coordinates block)) where

    passedUpLimit :: Coordinate -> Bool
    passedUpLimit (x, y) = y > numCellsHeight

isGameOver :: State -> Bool
isGameOver s = progress s == GameOver

isValidMovement :: Block -> Block -> Board -> Bool
isValidMovement previousBlock nextBlock board = foldr (&&) True (map isValidCoordinate (coordinates nextBlock)) where

    isValidCoordinate :: Coordinate -> Bool
    isValidCoordinate coord = hasCoordinate previousBlock coord || freePosition coord board

    freePosition :: Coordinate -> Board -> Bool
    freePosition coord board = Map.lookup coord board == Just Nothing

fullLinesErased :: State -> State
fullLinesErased s = updatedBoardState (clearAndCountFullLines numCellsHeight (board s)) where

    updatedBoardState :: (Board, Int) -> State
    updatedBoardState (board, count) = s {board = board, score = newScore} where
        newScore = (score s) + count * count * 10

clearAndCountFullLines :: Int -> Board -> (Board, Int)
clearAndCountFullLines 0 board = (board, 0)
clearAndCountFullLines n board 
    | isFullLine = addOne ((clearAndCountFullLines (n - 1)) (Map.union (downHigherLines n board) board))
    | otherwise  = clearAndCountFullLines (n - 1) board

    where
        isFullLine = Map.size (Map.filterWithKey (\(x, y) v -> y == n && v /= Nothing) board) == numCellsWidth

addOne :: (Board, Int) -> (Board, Int)
addOne (b, c) = (b, c + 1)

downHigherLines :: Int -> Board -> Board
downHigherLines n board = Map.union emptyLine (Map.mapWithKey (copyAboveLine board) board) where

    emptyLine = Map.fromList [ ((x, numCellsHeight), Nothing) | x <- [1..numCellsWidth] ]

    copyAboveLine :: Board -> Coordinate -> Maybe Tetrimino -> Maybe Tetrimino
    copyAboveLine board (x, y) tetr
        | y >= n && y < numCellsHeight = board Map.! (x, y + 1)
        | otherwise                    = tetr

updatedBoardState :: State -> State -> State
updatedBoardState newState oldState = newState {board = setBoardPosition (Just (shape newBlock)) clearBoard (coordinates newBlock)} where

    newBlock = currentBlock newState
    oldBlock = currentBlock oldState
    clearBoard = setBoardPosition Nothing (board oldState) (coordinates oldBlock)

    setBoardPosition :: Maybe Tetrimino -> Board -> [Coordinate] -> Board
    setBoardPosition value board coordinates = Map.union (Map.fromList (map (\c -> (c, value)) coordinates)) board



