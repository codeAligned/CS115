
-- csman username: rcasey
-- Assignment 1

-- Part A : Basic Exercises


-- Exercise A.1
-- part 1
-- Takes two arguments of type double, and returns some of squares
(+*) :: Double -> Double -> Double
x +* y = (x * x) + (y * y)
infixl 7 +*

-- part 2
-- Returns XOR (exclusive or) of two boolean inputs
(^||) :: Bool -> Bool -> Bool
x ^|| y = (not x && y) || (x && not y)
infixr 3 ^||


-- Exercise A.2
-- Takes 2 integer arguments, and computes product of all integers in the range
-- First argument must be smaller than the second argument for range to work
rangeProduct :: Int -> Int -> Int
rangeProduct x y | y < x = error "second argument must be larger than first"
                 | x == y = x
                 | otherwise = y * (rangeProduct x (y - 1))


-- Exercise A.3
-- Computes the product of all integers in the input list
prod :: [Int] -> Int
prod lst = foldr (*) 1 lst

-- Cleaner version to compute product of all integers in range of 2 int args
rangeProduct2 :: Int -> Int -> Int
rangeProduct2 x y = prod [x..y]

-- Exercise A.4
-- part 1
-- Takes a function f and two input lists, and returns a list with f applied
-- to both lists.
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _          = []
map2 _ _ []          = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys

-- part 2
-- Takes a function f and 3 input lists, and returns a list with f applied
-- to all three lists.
map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 _ [] _ _         = []
map3 _ _ [] _         = []
map3 _ _ _ []         = []
map3 f (a:as) (b:bs) (c:cs) = f a b c: map3 f as bs cs

-- part 3
{-  
    dot :: [Integer] -> [Integer] -> Integer
    dot = (sum .) . map2 (*)

-- f . g = \x -> f (g x)

    dot lst1 lst2
    ((sum .) . map2 (*)) lst1 lst2
    (\x -> (sum.) (map2 (*) x)) lst1 lst2  -- Replace middle . with lambda
    ((sum.) (map2 (*) lst1)) lst2   -- Apply lst1 to lambda expression
            -- where (map2 (*) lst1) = (\x -> map2 (*) lst1 x)
    ((sum .) (\x -> map2 (*) lst1 x) lst2)
    ((sum .) (map2 (*) lst1 lst2))  -- Apply lst2 to lambda expression
    ((\x -> sum . x) (map2 (*) lst1 lst2))
    ((\x -> sum (x)) (map2 (*) lst1 lst2))
    (sum (map2 (*) lst1 lst2))    
                   -- Since (map2 (*) lst1 lst2) is applied as an arg to sum 
                   -- Which is the same as the explicit version

-}

dot :: [Integer] -> [Integer] -> Integer
dot lst1 lst2 = (\x -> sum (x)) (map2 (*) lst1 lst2)

-- Exercise A.5
-- Computing sum of all integers less than 1000 that are multiples of 3 or 5
-- by running the following:
-- sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
-- --> result = 233168

-- Exercise A.6

-- Generates a list of prime numbers uing Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve [] = error "cannot sieve the empty list"
sieve (x:xs) =  x : (sieve $ filter (\a -> a `mod` x /= 0) xs)

-- Computing the sum of the primes before 10000 by running the following:
-- sum (takeWhile (<10000) (sieve [2..]))
-- answer --> 5736396


-- Part B: Pitfalls
 
-- Exercise B.1
-- The given version of sumList has bad coding style because it uses head and 
-- tail when pattern matching could be used instead, which makes it cleaner.
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList (xs)

-- Exercise B.2
-- We again have bad coding style because we use head and tail when pattern 
-- matching could be used instead to make the code cleaner.
largest :: [Integer] -> Integer
largest [] = error "empty list"
largest [x] = x
largest (x:xs) = max (x) (largest xs)


-- Part C: Evaluation

-- Exercise C.1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Evaluate fib 3
{-
fib 3
(fib (3-1) + fib (3-2))   -- pattern match fib n with n = 3
(fib (2) + fib (3-2))
((fib (2-1) + fib (2-2)) + fib(3-2)) -- pattern match fib n with n = 2
((fib (1) + fib (2-2)) + fib(3-2))
(((1) + fib (2-2)) + fib(3-2))  -- pattern match fib 1 (evaluates to 1)
(((1) + fib (0)) + fib(3-2))
(((1) + (0)) + fib(3-2)) -- pattern match fib 0 (evaluates to 0)
(1 + fib(3-2))
(1 + fib(1))
(1 + (1))  -- pattern match fib 1 (evaluates to 1)
==> 2
-}

-- Exercise C.2
--fact :: Integer -> Integer
--fact n = n * fact (n-1)
--fact 0 = 1

-- Evaluate fact 3
{- The given version of fact will never terminate because the pattern matching
will always match the case for n first, even if n = 0, because of the order
that it is given in.  Therefore, we can never reach the base case, and fact 3
will never terminate.

fact 3
(3 * fact (3-1))  -- pattern match fact n with n = 3
(3 * fact (2))
(3 * (2 * fact (2-1))) -- pattern match fact n with n = 2
(3 * (2 * fact (1)))
(3 * (2 * (1 * fact (1-1))))  -- pattern match fact n with n = 1
(3 * (2 * (1 * fact (0)))) 
(3 * (2 * (1 * (0 * fact (0-1)))))  -- pattern match fact n with n = 0
(3 * (2 * (1 * (0 * fact (-1)))))   
(3 * (2 * (1 * (0 * (-1 * fact (-1-1)))))) -- pattern match fact n with n = -1
(3 * (2 * (1 * (0 * (-1 * fact (-2))))))
clearly, this expression will never terminate.  Actually, if multiplication in 
Haskell worked by seeing the first operand as 0 and immediately knowing the
result would be 0, this could be evaluated to 0.  However, when I ran fact 3, 
we got stuck in an infinite loop, so apparently * doesn't work that way.
-}

-- we can fix by reversing the order of the pattern matching for 0 and n
-- a working version of fact:
fact2 :: Integer -> Integer
fact2 0 = 1
fact2 n = n * fact2 (n-1)

-- Exercise C.3

-- first given version of reverse
reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) = reverse xs ++ [x]

-- evaluate reverse2 [1,2,3]
{-
reverse [1,2,3]
(reverse[2,3] ++ [1])   -- pattern match [1,2,3] with (x:xs)
((reverse [3] ++ [2]) ++ [1])  -- pattern match [2,3] with (x:xs)
(((reverse [] ++ [3]) ++ [2]) ++ [1])  -- pattern match [3] with (x:xs)
((([] ++ [3]) ++ [2]) ++ [1])   -- pattern match [] with []
(([3] ++ [2]) ++ [1])    -- definition of ++. match [] ++ [3] with [] ys
((3 : ([] ++ [2])) ++ [1])  -- definition of ++
((3 : [2]) ++ [1])
(([3,2]) ++ [1])
(3 : ([2] ++ [1]))
(3 : (2 : ([] ++ [1])))
(3 : (2 : ([1])))
(3 : [2,1])
[3,2,1]
-}

-- second given version of reverse
reverse3 :: [a] -> [a]
reverse3 xs = iter xs []
  where
    iter :: [a] -> [a] -> [a]
    iter [] ys = ys
    iter (x:xs) ys = iter xs (x:ys)
-- evaluate reverse3 [1,2,3]
{-
(iter [1,2,3] [])    -- pattern match xs with [1,2,3]
(iter [2,3] (1:[]))
(iter [3] (2: (1:[])))
(iter [] (3: (2: (1:[]))))
(3: (2: (1:[])))  -- pattern match iter [] ys
(3: (2: [1]))
(3: ([2,1]))
[3,2,1]
-}

{-
While both versions have a maximum of three pending operations and thus similar
space complexities, the first version stores ++ operations, which then need to 
be evaluated with the definition of the (++) operator, which results in more
: operations.  But the second version removes the extra ++ operations by 
instantly just using the : operator to pass the updated list as an argument
to the recursive function.  The second version has a better time complexity
as overall there are fewer functions calls (perhaps the second version is 
O(n) while the first is O(n^2).  
-}

{-
If Haskell was a strictly evaluated language, it looks like the second version 
would be more space efficient, as we would evaluate the second argument to iter
and avoid having to store all of the pending : operations.  Instead, we could 
evaluate the stored list (the second argument to iter) to avoid having any
pending operations.  However, we wouldn't be able to do this with the first
version, as we would still have to reach down to the base case of the empty
list before applying the ++ operations.  So in this case, the second version
would be more space efficient as well as more time efficient.
-}


-- Exercise C.4

isort :: [Integer] -> [Integer]
isort [] = []
isort (n:ns) = insert n (isort ns)
  where
    insert :: Integer -> [Integer] -> [Integer]
    insert n [] = [n]
    insert n m@(m1:_) | n < m1 = n : m
    insert n (m1:ms) = m1 : insert n ms

-- Evaluate head (isort [3, 1, 2, 5, 4])
{-
head (insert 3 (isort [1, 2, 5, 4]) 
head (insert 3 (insert 1 (isort [2,5,4])))
head (insert 3 (insert 1 (insert 2 (isort [5,4]))))
head (insert 3 (insert 1 (insert 2 (insert 5 (isort [4])))))
head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort []))))))
head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 ([]))))))
head (insert 3 (insert 1 (insert 2 (insert 5 [4]))))
head (insert 3 (insert 1 (insert 2 (4: (insert 5 [])))))
head (insert 3 (insert 1 (2: (4: (insert 5 [])))))  -- since 2 < 4, 2nd pattern
head (insert 3 (1: (2: (4: (insert 5 [])))))  -- since 1 < 2, 2nd pattern
head (1: (insert 3 (2: (4: (insert 5 [])))))
1        -- pattern match (1: insert 3 [2,4,5]) with (x:_) in definition of head
-}


-- Exercise C.5
-- part 1
{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ init [] = init 
foldr f init (x:xs) = f x (foldr f init xs)
-}

-- Evaluate foldr max 0 [1, 5, 3, -2, 4]
{-
(max 1 (foldr max 0 [5,3,-2,4]))
(max 1 (max 5 (foldr max 0 [3,-2,4])))
(max 1 (max 5 (max 3 (foldr max 0 [-2,4]))))
(max 1 (max 5 (max 3 (max -2 (foldr max 0 [4])))))
(max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 []))))))
(max 1 (max 5 (max 3 (max -2 (max 4 0)))))
(max 1 (max 5 (max 3 (max -2 4))))
(max 1 (max 5 (max 3 4)))
(max 1 (max 5 4))
(max 1 5)
5
-}

-- part 2
{-
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ init [] = init
foldl f init (x:xs) = foldl f (f init x) xs
-}

-- evaluate foldl max 0 [1, 5, 3, -2, 4]
{-
(foldl max (max 0 1) [5,3,-2,4])
(foldl max (max (max 0 1) 5) [3,-2,4])
(foldl max (max (max (max 0 1) 5) 3) [-2,4])
(foldl max (max (max (max (max 0 1) 5) 3) -2) [4])
(foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) [])
(max (max (max (max (max 0 1) 5) 3) -2) 4)  -- pattern match with _ init []
(max (max (max (max 1 5) 3) -2) 4)
(max (max (max 5 3) -2) 4)
(max (max 5 -2) 4)
(max 5 4)
5
-}

{- If Haskell was stickly evaluated, foldl would be more space efficient, since 
we could evaluate each max expression on the fly without having to store any
further pending operations.  But foldr still would have to store all of the
operations until it reaches the base case, and then it could work its way
back up.  However, due to lazy evaluation, both foldr and foldl must store 
5 pending max operations.  Thus, in this case, they have very similar space
complexities.  However, in some circumstances, foldl can still be more efficient
even with lazy evaluation. -}


