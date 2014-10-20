--
-- Sudoku.hs
--

--
-- This program reads a Sudoku problem from a file and
-- outputs the solution to stdout.
--

module Main where

import Control.Monad
import Data.Array.IO
import Data.Char
import Data.List
import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = hPutStrLn stderr $ "usage: sudoku filename"

type Sudoku = IOArray (Int, Int) Int


-- Read a file's contents into a Sudoku array.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  let ls = lines s in
    if okLines ls
       then newListArray ((1, 1), (9, 9)) (map charToInt (concat ls))
       else error "readSudoku: invalid string input"
  where
    -- Check that the string input is a valid representation of a Sudoku board.
    okLines :: [String] -> Bool
    okLines ss =
      and [length ss == 9,
           all (\s -> length s == 9) ss,
           all okChar (concat ss)]

    okChar :: Char -> Bool
    okChar '.' = True
    okChar c | ord c >= ord '0' && ord c <= ord '9' = True
    okChar _ = False

    charToInt :: Char -> Int
    charToInt '.' = 0
    charToInt c   = ord c - ord '0'


-- Solve a Sudoku board.
-- Do this by iterating through the board, incrementing the unfilled numbers
-- by 1 until the right solution is found.
-- Return True if a solution is found, else false.
-- If a solution is found, the board contents will have mutated to the solution.
solveSudoku :: Sudoku -> IO Bool
solveSudoku s = iter s (1, 1)
   where
    -- Solve a Sudoku board starting from location (i, j).
    -- All "previous" locations are assumed to have been filled.
    -- If the board is solveable, return True; if not, return False.
    -- In the latter case the board will not have changed.
       iter :: Sudoku -> (Int, Int) -> IO Bool
       iter s (i, j) | i <= 9 = do c <- readArray s (i, j)
                                   values <- getOKValues s (i, j)
                                   if c == 0  -- try to solve if square is 0
                                     then do try <- iter' s (i, j) values
                                             if try
                                                then iter s (nextIndex i j)
                                                else return False
                                     else iter s (nextIndex i j) 
                     | otherwise = return True -- if we reach end of board
        
    -- Helper function to get to the next index of the board array.
    -- (checks if we run off the board horizontally, but caller must check
    -- if we run off the board vertically)
       nextIndex :: Int -> Int -> (Int, Int)
       nextIndex i 9 = (i + 1, 1) -- loop to next row
       nextIndex i j = (i, j + 1) -- go to next column


    -- Try to solve the board using all possible currently-valid
    -- values at a particular location.
    -- If the board is unsolveable, reset the location to a zero
    -- (unmake the move) and return False.
       iter' :: Sudoku -> (Int, Int) -> [Int] -> IO Bool
       iter' _ _ [] = return False -- if no more possible values
       iter' s (i, j) (x:xs) = do writeArray s (i, j) x -- try one integer
                                  valid <- iter s (i, j)
                                  if valid
                                     then iter s (nextIndex i j)
                                     else do writeArray s (i, j) 0 -- backtrack
                                             iter' s (i, j) xs -- try next value
                               
       

    -- Get a list of indices that could be in a particular location on the 
    -- board (no conflicts in row, column, or box).
       getOKValues :: Sudoku -> (Int, Int) -> IO [Int]
       getOKValues s (i, j) = do rowInts <- getRow s i
                                 colInts <- getCol s j
                                 boxInts <- getBox s (i, j)
                                 return $ [0..9] \\ (rowInts `union` colInts`union` boxInts)
                                  

    -- Return the ith row in a Sudoku board as a list of Ints.
       getRow :: Sudoku -> Int -> IO [Int]
       getRow s i = mapM (readArray s) [(i, x) | x <- [1..9]]

    -- Return the ith column in a Sudoku board as a list of Ints.
       getCol :: Sudoku -> Int -> IO [Int]
       getCol s i = mapM (readArray s) [(x, i) | x <- [1..9]]

    -- Return the box covering location (i, j) as a list of Ints.
       getBox :: Sudoku -> (Int, Int) -> IO [Int]
       getBox s (i, j) = mapM (readArray s) [(i' + x, j' + y) | x <- [1..3], y <- [1..3]]
                              where i' = ((i-1) `div` 3) * 3 -- i' is 0, 3, or 6
                                    j' = ((j-1) `div` 3) * 3 -- j' is 0, 3, or 6


-- Print a Sudoku board to stdout.
printSudoku :: Sudoku -> IO ()
printSudoku s = iter s 1 1
  where
    iter :: Sudoku -> Int -> Int -> IO ()
    iter s i j = 
      unless (i > 9)
        (do c <- readArray s (i, j)
            putChar $ intToChar c
            if j == 9 
               then putChar '\n' >> iter s (i + 1) 1
               else iter s i (j + 1))

    intToChar :: Int -> Char
    intToChar 0 = '.'
    intToChar n | n >= 1 && n <= 9 = intToDigit n
    intToChar m = error $ "printSudoku: invalid integer in array: " ++ show m


main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then usage >> exitFailure
     else
       do sudoku <- readSudoku (head args) -- read board contents into array
          solved <- solveSudoku sudoku
          if solved
             then printSudoku sudoku >> exitSuccess
             else putStrLn "No solution exists." >> exitFailure

