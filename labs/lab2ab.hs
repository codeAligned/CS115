{-# LANGUAGE DatatypeContexts #-}

module Lab2ab where
-- csman username: rcasey
-- Assignment 2 (Parts A and B)

-- Part A : Basic Exercises

-- Exercise A.1

-- Note: Manual instances of Show & Eq commented out (as per A.2)
{-
data Nat = Zero | Succ Nat
instance Show Nat where
  show Zero = "Zero"
  show (Succ n) = "(Succ " ++ show n ++ ")"
instance Eq Nat where
  Zero == Zero = True
  Succ n == Succ m = n == m
  _ == _ = False
-}

-- Exercise A.2
data Nat = Zero | Succ Nat
  deriving (Eq, Show)

-- Exercise A.3

instance Ord Nat where
  Zero <= _ = True
  Succ n <= Succ m = n <= m
  _ <= _ = False
{-
If we were to have Haskell derive Ord, Haskell would say that earlier 
constructors in the datatype declaration are smaller than later ones.
So in our case, we would say that Zero < (Succ Nat), which is true for
any Nat (since natural numbers must be greater than or equal to 0). So
deriving Ord should work fine in this case.
Deriving Ord won't work in some cases if there is no clear order we can
put the declarations in, or if we swap the order of the declarations such
that bigger ones are first, as Haskell doesn' know any better. 
-}

-- Exercise A.4
data SignedNat = Neg Nat | Pos Nat
  deriving (Show)

instance Eq SignedNat where
  Neg n == Neg m = n == m
  Pos n == Pos m = n == m
  Neg Zero == Pos Zero = True
  Pos Zero == Neg Zero = True
  _ == _ = False

instance Ord SignedNat where
  Neg _ <= Pos _ = True
  Neg n <= Neg m = n >= m
  Pos Zero <= Neg Zero = True
  Pos _ <= Neg _ = False
  Pos n <= Pos m = n <= m

  
{-
We couldn't exactly use the automatically dervied definitions for Eq and Ord 
because we would get some issues arising around the case of the natural 
number 0.  The derived definitions would say that Neg 0 < Pos 0 because 
the negative declaration appears first and is thus smaller, when in fact
we probably want Neg 0 = Pos 0 to be true.
Another problem is the treatment of negative numbers.  With the derived 
definition, we get things like Neg 10 < Neg 20 evaluate to True when that 
is clearly not the case, because the definitions have no notion of
"negative", and instead see 10 < 20 and evaluate to True.
-}

-- Exercise A.5

-- First, Num instance for regular natural numbers, allowing us to do some 
-- operations on Nats.
instance Num Nat where
    n + m = addNat n m
    n * m = mulNat n m

    abs 0 = Zero
    abs _ = (Succ (Zero))

    signum Zero = Zero
    signum _ = (Succ (Zero))

    

    fromInteger 0 = Zero
    fromInteger i | i > 0 = Succ . fromInteger $ i - 1
    fromInteger _ = error("Nat can't be negative")

-- Add two signed Natural Numbers together
addSignedNat :: SignedNat -> SignedNat -> SignedNat
addSignedNat (Neg Zero) n = n
addSignedNat n (Neg Zero) = n
addSignedNat (Pos Zero) n = n
addSignedNat n (Pos Zero) = n
addSignedNat (Pos m) (Pos n) = Pos (addNat m n)
addSignedNat (Neg m) (Neg n) = Neg (addNat m n)
addSignedNat (Pos m) (Neg n) | m > n = Pos (subNat m n)
                             | m == n = (Pos Zero)
                             | m < n = Neg (subNat n m)
addSignedNat (Neg m) (Pos n) | m > n = Neg (subNat m n)
                             | m == n = (Pos Zero)
                             | m < n = Pos (subNat n m)
addSignedNat (Pos (Succ m)) (Neg (Succ n)) = addSignedNat (Pos m) (Neg n)
addSignedNat (Neg (Succ m)) (Pos (Succ n)) = addSignedNat (Neg m)  (Neg n)

-- Multiplies two signed Natural Numbers together
multSignedNat :: SignedNat -> SignedNat -> SignedNat
multSignedNat (Neg Zero) _ = (Pos Zero) -- if one arg is 0, result is Zero
multSignedNat _ (Neg Zero) = (Pos Zero)
multSignedNat (Pos Zero) _ = (Pos Zero)
multSignedNat _ (Pos Zero) = (Pos Zero)
multSignedNat (Pos m) (Pos n) = Pos (mulNat m n)
multSignedNat (Pos m) (Neg n) = Neg (mulNat m n)
multSignedNat (Neg m) (Pos n) = Neg (mulNat m n)
multSignedNat (Neg m) (Neg n) = Pos (mulNat m n)


-- Negates a signed Natural Number
negSignedNat :: SignedNat -> SignedNat
negSignedNat (Pos m) = (Neg m)
negSignedNat (Neg m) = (Pos m)

-- Takes the absolute value of a signed Natural Number (makes it positive)
absSignedNat :: SignedNat -> SignedNat
absSignedNat (Pos m) = (Pos m)
absSignedNat (Neg m) = (Pos m)

-- Returns the sign of a signed Natural Number (-1, 1, or 0)
signSignedNat :: SignedNat -> SignedNat
signSignedNat (Pos Zero) = (Pos Zero)
signSignedNat (Neg Zero) = (Neg Zero)
signSignedNat (Pos _) = (Pos (Succ (Zero)))
signSignedNat (Neg _) = (Neg (Succ (Zero)))

-- Converts an integer (positive or negative) to a Signed Nat
integerToSignedNat :: Integer -> SignedNat
integerToSignedNat 0 = (Pos Zero)
integerToSignedNat n | n > 0 = (Pos (integerToNat n))
integerToSignedNat n = (Neg (integerToNat (abs n)))

-- Num instance of SignedNat, using all of the helper functions above
-- (Note: In general, our helper functions will (in general) result in 
-- (Pos Zero) when there is a choice between which sign of Zero to return).
instance Num SignedNat where
    m + n = addSignedNat m n
    m * n = multSignedNat m n
    negate m = negSignedNat m
    abs m = absSignedNat m
    signum m = signSignedNat m
    fromInteger m = integerToSignedNat m

-- Helper function to add two regular Nats
addNat :: Nat -> Nat -> Nat
addNat Zero n = n
addNat (Succ m) n = Succ (addNat m n)

-- Helper function to subtract two regular Nats
subNat :: Nat -> Nat -> Nat
subNat Zero Zero = Zero
subNat Zero _ = error("Cannot subtract a nonzero natural number from zero")
subNat n Zero = n
subNat (Succ m) n = integerToNat ((natToInteger (addNat m (Succ Zero))) - (natToInteger n))

-- Helper function to multiply two regular Nats
mulNat :: Nat -> Nat -> Nat
mulNat Zero _ = Zero
mulNat (Succ m) n = addNat n (mulNat m n)

-- Helper function to convert a Nat to an integer
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

-- Helper function to convert an integer to a Nat
integerToNat :: Integer -> Nat
integerToNat n | n < 0 = error "invalid argument"
integerToNat 0 = Zero
integerToNat n = Succ $ integerToNat (n - 1)


-- Exercise A.6
signedNatToInteger :: SignedNat -> Integer
signedNatToInteger (Pos Zero) = 0
signedNatToInteger (Neg Zero) = 0
signedNatToInteger (Pos m) = natToInteger m
signedNatToInteger (Neg m) = (-1) * (natToInteger m)

-- Exercise A.7
{-
SignedNat is ugly and redundant in that in has two representations of the value
0: (Pos Zero) and (Neg Zero).  This seems entirely unnecessary, and we have to
put in checks for both of these in all of our functions, and then we have to 
arbitraily decide which one we should return if we want Zero (which shouldn't
really have a sign in the first place).

We could define a new Data Type that could represent negative numbers, 0, and 
positive numbers all seperately to avoid this problem.

data UnaryInteger = Neg Unary | Zero | Pos Unary
  deriving (Show)

Where we have a representation of Unary for the numbers 1,2,3...

But in this case, we have an extra declaration that we need to handle, as well 
as the new need to represent 1,2,3 (like Nats, but starting at 1 instead of 0), 
which should bedo-able using the same notion of Succ, but instead starting at 1.

This would be better in that we would only have one representation of Zero, but 
more difficult in that we would have to define Unary as follows:

data Unary = 1 | Succ Unary
  deriving (Show, Eq, Ord)
-}

-- Exercise A.8
factorial :: (Num a, Ord a) => a -> a
factorial 0 = 1
factorial n | n < 0 = error("Can't compute factorial of a negative!")
factorial n = n * factorial(n-1)

{-
Note that this works for Signed Nats:
factorial (Pos (Succ (Succ (Succ Zero))))
= Pos (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))  

So the factorial of 3 is 6, as expected.
-}


-- Part B: Pitfalls


-- A working version of Tree (without the Ord constraint)
data Tree a =
    Leaf
  | Node a (Tree a) (Tree a)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node x left right) = 
    Node (f x) (fmap f left) (fmap f right)

-- Ben Bitdiddle's verision which causes compiler errors when we try to write
-- the instance of Functor for Tree.
{-
data Ord a => Tree a =
    Leaf
  | Node a (Tree a) (Tree a)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node x left right) = 
    Node (f x) (fmap f left) (fmap f right)
-}

{-
The problem here is that Functor is defined as follows:

class Functor f where
  fmap :: (a -> b) -> f a -> f b

We see that there are no constraints on the Functor class, as fmap must work
for anything (regardless if it is constrained by Ord or not).  To be able to
use Functor we must be able to take any function of any type signature, 
regardless of it is Ord or not.  Thus you cannot write a Functor
instance for a data type that has a constraint, because we are restricting to
only functions that are order-able, it won't type check because it is not
compatible with Functor.  Thus, we have a more fundamental problem. 

The only way to keep the Ord constraint would be to rewrite the Functor class 
to be a "constrained Functor class" such that map would work for the Ord 
constraint:

class Functor f => OrdFunctor f where
    smap :: (Ord a, Ord b) => (a -> b) -> f a -> f b
    smap f = fmap f

But unfortunately the normal Functor is not written in such a way that would
make it easy to add constraints like Ord to Tree if we wanted to do a Functor
instance of Tree.  However, there are ways to get around this limiting factor 
of Functor, as seen above.
-}
