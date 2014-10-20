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
           positive = all isPosInteger (init args) -- True iff n1 n2... are all positive integers
       in if positive == False
              then usage >> exitFailure 
              else
                let cols = map readInt (init args)
                    -- function to abstract code that was repeated in both "then" and "else"
                    printCols = \ x -> putStrLn (unlines (map unwords (map (filterLines cols) 
                            (map words (lines x))))) >> exitSuccess
                in do
                  if file == "-"      -- read from stdin if filename is given as "-"
                   then do contents <- hGetContents stdin
                           printCols contents
                   else do          -- else, get our contents from the input file
                        contents <- readFile file
                        printCols contents

-- Parse a string into a int.
readInt :: String -> Int
readInt x = read x :: Int

-- Determine if a string represents a positive interger (composed of Digits)
isPosInteger :: String -> Bool
isPosInteger n = all isDigit n
        


-- Takes a list of of columns and a single lines (decomposed to words), and
-- returns only the selected columns
-- (Note: Since lists are 0-indexed, and we are treating the columns as 
-- 1-indexed, we must decrease each item in cols by 1.)
-- (Note 2: Also, we need to filter out indices that are bigger than the length
-- of the list. Then filter out indices that are smaller than 1 [even though 
-- these should already result in an error message])
filterLines :: [Int] -> [String] -> [String]
filterLines n words = map (words !!) (map (subtract 1) (filter (> 0) 
            (filter (<= (length words)) n)))
