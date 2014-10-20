--
-- Columns.hs
--

--
-- This program reads lines from a file and outputs the words in the 
-- input columns of each line to stdout.
--

module Main where

import System.Environment 
import System.Exit
import System.IO
import Data.Char

-- print a usage error message if wrong number of arguments is given
-- note that there must be at least 1 column given, or else we signal an error.
-- This means that there must be at least 2 arguments.
usage :: IO ()
usage = putStrLn "usage: columns n1 n2 ... filename"

-- prints out the columns (n1 n2...) of the input file, ignoring columns that
-- are out of bounds of each line in the file.
main :: IO ()
main = do
  args <- getArgs
  if length args <= 1  -- we need at least 1 column and the filename
     then usage >> exitFailure
     else 
       let file = last args
           cols = map readInt (init args)
       in do
          if file == "-"      -- read from stdin if filename is given as "-"
             then do contents <- hGetContents stdin
                     putStrLn (unlines (map unwords (map (filterLines cols) 
                            (map words (lines contents))))) >> exitSuccess
             else do          -- else, get our contents from the input file
                contents <- readFile file
                putStrLn (unlines (map unwords (map (filterLines cols) 
                            (map words (lines contents))))) >> exitSuccess

-- Parse a string into a int.
readInt :: String -> Int
readInt x = read x :: Int

{-
checkPosInts :: [String] -> Bool
checkPosInts [] = True
checkPosInts (n:ns) | readInt n = checkPosInts ns
                    | otherwise = False
-}
        


-- Takes a list of of columns and a single lines (decomposed to words), and
-- returns only the selected columns
-- (Note: Since lists are 0-indexed, and we are treating the columns as 
-- 1-indexed, we must decrease each item in cols by 1.)
-- (Note 2: Also, we need to filter out indices that are too small or bigger
-- than the length of the list)
filterLines :: [Int] -> [String] -> [String]
filterLines [] _ = []
filterLines (n:ns) words | n <= 0 || n > (length words)  = 
                    [] : (filterLines ns words)
filterLines (n:ns) words = (words !! (n-1)) : (filterLines ns words)
         
