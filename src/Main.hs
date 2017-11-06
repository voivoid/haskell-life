module Main where

import qualified System.IO
import qualified System.Console.ANSI as Console
import qualified Control.Concurrent
import qualified Control.Exception
import qualified System.Posix.Signals as Signals

import Control.Exception.Base(assert)
import Control.Monad(when)
import Data.List(nub)


type Pos = (Int, Int)
type Cell = Pos

data Board = Board { getAliveCells :: [Cell] }

emptyBoard :: Board
emptyBoard = Board []

gliderBoard :: Board
gliderBoard = Board [ (4, 2), (2, 3), (4, 3), (3, 4), (4, 4) ]

boardWidth :: Int
boardWidth = 20

boardHeight :: Int
boardHeight = 10

generationTimer :: Int
generationTimer = 250000

cellSymbol :: Char
cellSymbol = 'o'

putCell :: Pos -> IO()
putCell (x,y) = do
  Console.setCursorPosition y x
  putChar cellSymbol

showBoard :: Board -> Int -> IO()
showBoard board generation = do
  Console.clearScreen
  sequence $ map putCell $ getAliveCells board
  Console.setCursorPosition boardHeight 0
  putStrLn $ "Generation: " ++ show generation
  System.IO.hFlush System.IO.stdout

cellNeighbors :: Pos -> [Pos]
cellNeighbors (x,y) = map wrap neighbors
                      where neighbors = [ (x-1, y-1), (x, y-1), (x+1, y-1),
                                          (x-1, y  ),           (x+1, y  ),
                                          (x-1, y+1), (x, y+1), (x+1, y+1) ]
                            wrap (x,y) = (x', y')
                                         where x' = if x < 0 then boardWidth - 1 else if x >= boardWidth then 0 else x
                                               y' = if y < 0 then boardHeight - 1 else if y >= boardHeight then 0 else y

isAlive :: Board -> Pos -> Bool
isAlive board pos = elem pos $ getAliveCells board

isEmpty :: Board -> Pos -> Bool
isEmpty board = not . ( isAlive board )

getFreeCells :: Board -> [Pos]
getFreeCells board = filter (willBorn board) freeNeighbors
                     where allNeighbors = nub $ concat $ map cellNeighbors $ getAliveCells board
                           freeNeighbors = filter (isEmpty board) allNeighbors



countAliveNeighbors :: Board -> Pos -> Int
countAliveNeighbors board pos = length $ filter ( isAlive board ) $ cellNeighbors pos

willSurvive :: Board -> Pos -> Bool
willSurvive board pos = assert (isAlive board pos) $
                        aliveNeighbors == 2 || aliveNeighbors == 3
                        where aliveNeighbors = countAliveNeighbors board pos

willBorn :: Board -> Pos -> Bool
willBorn board pos = assert (isEmpty board pos) $
                     countAliveNeighbors board pos == 3

updateBoard :: Board -> Board
updateBoard board = Board ( survivors ++ newborn )
                    where survivors = filter (willSurvive board) $ getAliveCells board
                          newborn = filter (willBorn board) $ getFreeCells board

validateBoard :: Board -> Bool
validateBoard board = all (<boardWidth) xs && all (<boardHeight) ys
                      where (xs, ys) = unzip $ getAliveCells board

play :: Board -> Int -> IO ()
play board generation = do
  when (not $ validateBoard board) (error "Invalid board")

  showBoard board generation
  Control.Concurrent.threadDelay generationTimer
  play (updateBoard board) (generation + 1)

ctrlCHandler :: Control.Concurrent.ThreadId -> IO()
ctrlCHandler tid = do
  Console.showCursor
  Control.Exception.throwTo tid Control.Exception.UserInterrupt

main = do
  Console.hideCursor
  mainThreadId <- Control.Concurrent.myThreadId
  Signals.installHandler Signals.keyboardSignal (Signals.Catch $ ctrlCHandler mainThreadId) Nothing
  play gliderBoard 0
