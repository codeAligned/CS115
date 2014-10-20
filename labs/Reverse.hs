--
-- Reverse.hs
-- 

--
-- This program reads a file and outputs the lines of the file
-- in reverse order to stdout.
--

module Main where

import System.Environment 
import System.Exit

-- print a usage error message if wrong number of arguments is given
usage :: IO ()
usage = putStrLn "usage: reverse filename"


main :: IO ()
main = do
  args <- getArgs
  if length args /= 1   -- we expect only 1 argument (the filename)
     then usage >> exitFailure
     else let
          file = head args
          in do
             contents <- readFile file
             putStr (unlines (reverse (lines contents))) >> exitSuccess

         
