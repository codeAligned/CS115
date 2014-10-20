module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S 

data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: (M.Map (Integer, Integer) a) }  -- values
  deriving (Eq, Show)

-- Exercise C.1

sparseMatrix :: (Eq a, Num a) => 
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
-- sparseMatrix <list of index/element pairs> <bounds> -> sparse matrix
sparseMatrix _ (n, m) | n < 1 || m < 1 = error("Invalid bounds")
sparseMatrix lst (n,m) | checkValid lst (n,m) == False = error("Index not within bounds")
sparseMatrix lst (n,m) = SM {bounds = (n,m), rowIndices = getRows lst, colIndices = getCols lst,
             vals = makeValues lst}


-- Goes through each list/index pair and checks if they are wihtin the bounds
checkValid :: (Eq a, Num a) => [((Integer, Integer), a)] -> (Integer, Integer) -> Bool
checkValid lst (n,m) = all (\ ((r,c), _) -> r <= n && c <= m) lst

-- Makes the values and filters out the 0 elements
makeValues :: (Eq a, Num a) => [((Integer, Integer), a)] -> (M.Map (Integer, Integer) a)
makeValues lst = M.filter (/= (fromInteger 0)) (M.fromList lst)

-- Get row indices containing nonzero elements
getRows :: (Eq a, Num a) => [((Integer, Integer), a)] -> S.Set Integer
getRows lst = S.fromList (map (\ ((r,_), _) -> r) (filter ((/= (fromInteger 0)).snd) lst))

-- Get column indices containing notzero elements
getCols :: (Eq a, Num a) => [((Integer, Integer), a)] -> S.Set Integer
getCols lst = S.fromList (map (\ ((_,c), _) -> c) (filter ((/= (fromInteger 0)).snd) lst))


-- Exercise C.2

-- Adds two sparse matrices together, resulting in another sparse matrix
-- Filters out all new 0 elements
addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM smA smB | bounds smA /= bounds smB = error("Noncompatible matrices")
addSM smA smB = sparseMatrix (M.toList (M.unionWith (+) (vals smA) (vals smB))) (bounds smA)

-- Exercise C.3

-- Negates all of the values in a sparse matrix
negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM smA = sparseMatrix (M.toList (M.map negate (vals smA))) (bounds smA)

-- Exercise C.4

-- Subtracts two sparse matrices, resulting in another sparse matrix
-- Filters out all new 0 elements
subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM smA smB | bounds smA /= bounds smB = error("Noncompatible matrices")
subSM smA smB = sparseMatrix (M.toList (M.filter (/= (fromInteger 0)) (M.unionWith (-) 
         (vals smA) (vals smB)))) (bounds smA)

-- Exercise C.5

-- Multiply two sparse matrixes together, by multiplying together only those rows and
-- columns that contain nonzero elements. New bounds will be the number of rows from the
-- first matrix and the number of columns from the second matrix.
mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM smA smB | checkMultBounds smA smB == False = 
      error("Matrices not compatible for multiplication")
mulSM smA smB = sparseMatrix [((x,y), (multRowCol smA smB x y)) | x <- S.toList (rowIndices smA), 
      y <- S.toList (colIndices smB)] (fst (bounds smA), snd (bounds smB))

-- Checks if the bounds are right for a multiplication operation
-- (The number of columns in the first matrix must equal the number of rows in the second)
checkMultBounds :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> Bool
checkMultBounds smA smB = ((snd (bounds smA)) == (fst (bounds smB)))

-- Returns the dot product of a row of the first matrix and a column of the second matrix
multRowCol :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> Integer -> Integer -> a
multRowCol smA smB x y = sum (M.elems (M.intersectionWith (*) (M.mapKeys (\ (_,c) -> c)
                   ((M.filterWithKey (\ (r,_) _ -> r == x) (vals smA))))
          (M.mapKeys (\ (r,_) -> r) (M.filterWithKey (\ (_,c) _ -> c == y) (vals smB)))))

-- Exercise C.6
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM _ (n,m) | (n < 1) || (m < 1) = error("Index out of bounds")
getSM smA (n,m) | (n > (fst (bounds smA)) || m > (snd (bounds smA)))
       = error("Index out of bounds")
getSM smA (n,m) = M.findWithDefault 0 (n,m) (vals smA)


-- Returns the number of rows in a sparse matrix
rowsSM :: (Eq a, Num a) => SparseMatrix a -> Integer
rowsSM smA = fst (bounds smA)

-- returns the number of columns in a sparse matrix
colsSM :: (Eq a, Num a) => SparseMatrix a -> Integer
colsSM smA = snd (bounds smA)

-- Exercise C.7

-- <+> is an operator shortcut for adding two sparse matrices
(<+>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<+>) smA smB = addSM smA smB

-- <-> is an operator shortcut for subtracting two sparse matrices
(<->) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<->) smA smB = subSM smA smB

-- <*> is an operator shortcut for multiplying two sparse matrices
(<*>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<*>) smA smB = mulSM smA smB

-- <!> is an operator shortcut for getSM
(<!>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<!>) smA (n,m) = getSM smA (n,m)

-- Exercise C.8
{-
We can't define the Sparse Matrix datatype as an instance of the Num type class 
because we can't support all of the operations needed for a complete definition.
In particular, there is no abs or signum functions that are appropriate for 
a SparseMatrix.  Thus, we can't use an instance of Num here.
-}

-- Simple matrices used for testing
smA = sparseMatrix [((1,1),1)] (1,1) 
smB = sparseMatrix [((1,1),3)] (1,1) 

sm1 = sparseMatrix [((1,18),18),((2,14),62),((3,10),68),((3,14),47),((3,18),40),((4,2),50),((4,10),97),((4,13),69),((5,7),4),((5,11),90),((5,19),81),((6,4),92),((7,10),36),((8,3),40),((8,20),28)] (10,20)
sm2 = sparseMatrix [((2,2),42),((5,12),69),((6,11),14),((7,9),74),((8,10),82),((8,11),78),((9,7),57),((10,6),43),((10,13),89),((10,14),46),((11,7),6),((13,1),75),((13,8),24),((19,3),14),((19,13),97)] (20,15)


