--
-- Assignment 4
--

--
-- csman username: rcasey
--

module Main where

import Control.Monad
import Control.Monad.State
import Data.IORef

-- Part A

-- Exercise A.1

{- Ben Bitdiddle's code:

hr_comp :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_comp =
  [((i, l), (j, k), i^3 + l^3) | 
   i <- [1..], 
   j <- [1..i-1], 
   k <- [1..j-1], 
   l <- [1..k-1], 
   i^3 + l^3 == j^3 + k^3]
-}

-- Rewrite Ben's code using list monad and guard
-- Takes no arguments, and returns a list of positive integers that can be
-- expressed as the sum of cubes in two different ways.  The list is composed
-- of elements containing two tuples (of the integers to cube) and the integer
-- that is the sum of the cubes.
hr_comp :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_comp = do i <- [1..]
             j <- [1..i-1]
             k <- [1..j-1]
             l <- [1..k-1]
             guard $ i^3 + l^3 == j^3 + k^3
             return ((i, l), (j, k), i^3 + l^3)

-- Exercise A.2
-- Takes no arguments, and uses the list monad to sum all the integers
-- smaller than 1000 that are multiples of 3 or 5.
multiples :: Integer
multiples = sum (do x <- [1..999]       -- consider all integers less than 1000
                    if x `mod` 3 == 0 || x `mod` 5 == 0  -- if div by 3 or 5
                       then return x     -- add to list
                       else mzero)       -- else, don't add

-- Result = 233168


-- Exercise A.3

-- Returns True if the given Integer is a Palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n) -- see if read same backwards

largestPalindrome :: Integer
largestPalindrome = maximum (do i <- [100..999] -- consider all integers 100 to 1000
                                j <- [i..999]  -- (avoid looking at pairs twice)
                                guard $ isPalindrome (i*j) -- get palindromes
                                return (i*j)) -- consider product, find largest

-- The largest such palindrome is 906609


-- Part B: Puzzles and Derivations

-- Exercise B.1
{-
do n1 <- [1..6]
   n2 <- [1..6]
   []
   return (n1, n2)

desugars into:
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> [] >> return (n1, n2))
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> [] >>= \ _ -> return (n1, n2))
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> concat (map  (\ _ -> return (n1, n2)) []))
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> [])
[1..6] >>= \n1 -> (concat (map (\n2 -> []) [1..6]))
concat (map (\n1 -> (concat (map (\n2 -> []) [1..6]))) [1..6]) 
concat (map (\n1 -> (concat [[],[],[],[],[],[]])) [1..6])
concat (map (\n1 ->[]) [1..6])
concat [[],[],[],[],[],[]]
[]

so the result is []

-}

-- Exercise B.2

{-
do n1 <- [1..6]
   n2 <- [1..6]
   return <anything>
   return (n1, n2)

desugars into:
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> return <anything> >> return (n1, n2))
[1..6] >>= \n1 -> ([1..6] >>= 
       \n2 -> return <anything> >>= \ _ -> return (n1, n2))
[1..6] >>= \n1 -> ([1..6] >>= 
       \n2 -> concat (map (\ _ -> return (n1, n2)) (return <anything>)))
[1..6] >>= \n1 -> ([1..6] >>= 
       \n2 -> concat (map (\ _ -> return (n1, n2)) ([<anything>])))
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> concat [[(n1, n2)]])
[1..6] >>= \n1 -> ([1..6] >>= \n2 ->[(n1, n2)])

compared to:
do n1 <- [1..6]
   n2 <- [1..6]
   return (n1, n2)

which desugars into:
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> return (n1, n2))
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> [(n1, n2)])

So both expressions evaluate to the same thing.
-}


-- Exercise B.3

{-
let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
  do ['a', 'a', c1, c2, 'b', 'b'] <- s 
     return [c1, c2]


desugars into:
["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] >>=
           (\y -> case y of
               ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
               _ -> fail "user error (Pattern match failure in do expression)")
concat (map (\y -> case y of
                ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
                _ -> fail "user error (Pattern match failure in do expression)")
            ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"])
concat (map (\y -> case y of
                ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
                _ -> []) ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"])
concat ([return ["x", "y"], return ["z", "w"], [], return ["c", "c"], []])
concat ([[["x", "y"]], [["z", "w"]], [], [["c", "c"]], []])
["xy", "zw", "cc"]

If the list monad instead used the regular fail method of the Monad type class
(fail s = error s), then everytime we had a string that failed to pattern
match, we would get an error, and the program would stop.  But in reality, 
we simply want to ignore those cases, so putting an empty list instead
of a error message fixes that problem, allowing us to go through all the 
elements of s, ignoring those that fail to pattern match.
-}

-- Exercise B.4

{- Ben Bitdiddle's code:

{-# LANGUAGE ExistentialQuantification #-}

module Sum where

data AnyNum = forall a . Num a => AnyNum a

anySum :: [AnyNum] -> AnyNum
anySum [] = AnyNum 0
anySum ((AnyNum n) : ns) =
  case anySum ns of
    AnyNum s -> AnyNum (n + s)
-}

{-
The problem here is that AnyNum accepts any instance of Num as its type.  
However, at compile time, it is impossible to use the "+" operator to add
instances of Num of different types, so GHC complains.  To fix this, we would
need to add the restriction that all of the elements in [AnyNum] be of the same
type so the "+" operator could be used.  However, this completely defeats the
purpose of AnyNum in the first place.  It is impossible to fix this issue 
without changing the AnyNum datatype, as we can't restrict which types we might
try to add together, but we also don't have a "+" operator that accepts
instances of different types. 
-}


-- Part C: IORefs and State Monads

-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO () 
whileIO test block = 
  do b <- test 
     when b (block >> whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body = 
  do s0 <- get
     when (test s0)
          (do modify (snd . runState body)
              whileState test body)

-- Exercise C.1

-- Takes a nonnegative integer and computes its factorial in the IO monad
factIO :: Integer -> IO Integer
factIO n | n < 0 = error "Argument must be positive"
factIO n =
  do r <- newIORef 1   -- r is the running total
     i <- newIORef n   -- i is the counter
     whileIO
       (do i' <- readIORef i
           return (i' /= 0))   -- while i is not 0...
       (do r' <- readIORef r
           i' <- readIORef i
           writeIORef r (r' * i')  -- multiply running total by i
           writeIORef i (i' - 1))   -- decrement i by 1
     readIORef r   -- return running total
                    

-- Exercise C.2

-- Takes a nonnegative integer and computes its factorial using the 
-- State monad.
factState :: Integer -> Integer
factState n | n < 0 = error "Argument must be positive"
factState n = evalState factorialState (n, 1)

-- The state is composed of two integers, the first representing a 
-- counter, and the second representing the running total.
factorialState :: State (Integer, Integer) Integer
factorialState = do 
               whileState       -- while counter is not 0...
                 (\(n, _) -> n /= 0)
                 (do (n, i) <- get
                     put (n - 1, i * n)) -- decrement counter, multiply total.
               (_, i) <- get
               return i         -- return total

-- Exercise C.3

-- Takes a nonnegative integer n and computes the nth fibonacci number
-- using the IO monad.
fibIO :: Integer -> IO Integer
fibIO n | n < 0 = error "Argument must be positive"
fibIO n =
  do n1 <- newIORef 0    -- n1 is the second to last fibonacci number computed
     n2 <- newIORef 1    -- n2 is the last fibonacci number computed
     i <- newIORef n     -- i is the counter
     whileIO
       (do i' <- readIORef i    -- while i is not 0...
           return (i' /= 0))
       (do n1' <- readIORef n1
           n2' <- readIORef n2
           i' <- readIORef i
           writeIORef n1 n2'    -- second to last becomes last computed
           writeIORef n2 (n1' + n2') -- compute next fib number
           writeIORef i (i' - 1)) -- decrement counter
     readIORef n1     -- return smaller computed fib number

-- Exercise C.4

-- Takes a nonnegative integer n and computes the nth fibonacci number
-- using the State Monad. 
fibState :: Integer -> Integer
fibState n | n < 0 = error "Argument must be positive"
fibState n = evalState fibonacciState (0, 1, n)

-- The state is composed of three integers, the first representing a 
-- the second to last fibonacci number computed, the second representing
-- the last fibonacci number computed, and the third representing the 
-- counter, which is decremented by 1 each time through the loop.
fibonacciState :: State (Integer, Integer, Integer) Integer
fibonacciState = do 
               whileState 
                 (\(_, _, n) -> n /= 0)   -- while counter is not 0..
                 (do (f1, f2, n) <- get            -- decrement counter and..
                     put (f2, f1 + f2, n - 1)) -- ..computer new fib numbers
               (f1, _, _) <- get
               return f1  -- return smaller computed fib number





