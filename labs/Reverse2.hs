module Main where

import System.Environment 
import System.Exit

-- reverses the order of lines in an input file, printing the result
main :: IO () 
main = do 
     args <- getArgs
     case args of
        fs | length (args) == 1 -> do
            concat `fmap` mapM readFile fs >>= putStr . unlines. reverse . lines
            exitWith ExitSuccess
     -- if we don't have exactly 1 argument, print a usage message and exit
        _ -> do
            putStrLn "usage: reverse filename"
            exitWith (ExitFailure 1)

         
