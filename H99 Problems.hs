import Data.List
import System.Random
-- HASKELL 99 QUESTIONS

-- ====================
--LIST PROBLEMS (1-10):
-- ====================

-- Problem 1 (Find the last element of a list)
problem1 :: [a] -> a   -- Generic, list of type a -> type a
problem1 = last
problem [] = []

-- Problem 2 (Find the second last element in a list)
problem2 :: [a] -> a
problem2 x 
    | null x = error "The list is empty"
    | length x == 1 = error "The list is too short"
    | length x == 2 = head x
    | otherwise = (problem2.tail) (x) -- function composition + recursion

-- Problem 3 (Find the kth element of a list (starting from 1))
problem3 :: [a] -> Int -> a
problem3 x y = x!!(y -1)

problem3' x y 
    | y < 1 = error "Cannot do negative indexes"
    | (length x) < y = error "Index beyond list range"
    | otherwise = x!!(y - 1)



-- Problem 4 (Find the number of elements in a list)
problem4 :: [a] -> Int
problem4 = length

problem4' [] = 0
problem4' (_:x) = 1 + problem4' x

-- Problem 5 (reverse a list)
problem5 :: [a] -> [a]
problem5 = reverse

problem5' x 
    | x == [] = []
    | otherwise =  (problem5'.tail) (x) ++ [x!!0]

-- Problem 6 (Determine if a given list is a palindrome)
problem6 :: (Eq a) => [a] -> Bool -- (Eq a) => says a is of a type that is a subset of Eq so '==' operator can be used. This type is used in a list, then a bool returned.
problem6 x = x == reverse x


-- Problem 7 (Flatten a list of nested single element lists, e.g [1,[2,[3,[4]]]] -> [1,2,3,4])
data NestedList a = Elem a | List [NestedList a] -- Define a data type recursively

problem7 :: NestedList a -> [a] -- Input nestedlist output list
problem7 (Elem x) = [x] --If a single element, return a list of it, stop recursion
problem7 (List(x:xs)) = problem7 x ++ problem7(List xs) -- Take first element, create list, recusively deal with the remainder of the list
problem7 (List []) = [] -- When list empty, send empty back stop recursion

-- Test data in form:   problem7 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

-- Problem 9 (Remove consecutive duplicates from a list)
problem8 :: Eq a => [a] -> [a] -- In order to check if they belong to a group, must use '==' which is is property of Eq types classes.
problem8 = map head.group -- partial application of map, group groups consecutive same elements together, take first of each group (head).

-- Alternative recursive below, split list into x:ys where y is the first element of ys. If x == y then get rid of x (so no duplicates), else keep x, work recusively on ys.
problem8' (x:ys@(y:_)) -- Pattern matching x ys (where ys = y:_)
    | x == y    = problem8' ys
    | otherwise = x : problem8' ys
problem8' ys = ys

--Alternative approach, use dropwhile to remove adjacent repeats of x from xs
problem8'' [] = []
problem8'' (x:xs) = x : (problem8'' $ dropWhile (==x) xs) -- Split list into x:xs, then add x to the recusion of xs, with the front consecutive x values being dropped (removed).

-- Problem 9 (Group consecutive elements into lists within a list)
problem9 :: (Eq a) => [a] -> [[a]]
problem9 = group --lazy (I know) but it works okaaaayy

-- Problem10 (Group elements, return tuples of the repeats and the value)
problem10 :: (Eq a) => [a] -> [(Int, a)]
problem10 xs = [(length x, head x) | x <- group xs] -- For each collection of elements in group xs, take number, and the value and put in a tuple.


-- ====================
--LIST PROBLEMS CONTINUED (11-20):
-- ====================

-- Problem 11 (Modify problem 10 such that it differntiates between single, repeats and for repeats the number)
data Listthing a = Single a | Multiple Int a
    deriving (Show) -- So that it can be printed/shown

problem11 :: Eq a => [a] -> [Listthing a]
problem11 xs = [y | x <- group xs, let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)] -- For each group determine which of the union Listthing is used.

-- alternatively, using problem10 as a base.

problem11' xs = map (changeformat) (problem10 xs)
    where
        changeformat (1, x) = Single x
        changeformat (n, x) = Multiple n x

-- Problem 12 (decode the RLE)
problem12 :: [Listthing a] -> [a]
problem12 xs = concatMap decode xs
    where 
        decode (Single x) = [x]
        decode (Multiple n x) = replicate n x

-- Problem 13 (Encode RLE) SKIPPED WAS TOO SIMILAR TO PREVIOUS PROBLEMS

-- Problem 14 (Duplicate items in a list)
problem14 :: [a] -> [a]
problem14 xs = concat [[y,y] | y <- xs]

-- Alternatively using pattern matching
problem14' (x:xs) = x:x:problem14' xs -- ':' adds together, recursive

-- Problem 15 (make it repeat elements by any number)
problem15 :: [a] -> Int -> [a]
problem15 xs n= concat [replicate n x | x <- xs]

-- alternatively
problem15' xs n = concatMap(replicate n) xs

problem15'' :: [b] -> Int -> [b]
problem15'' xs n = xs >>= replicate n -- Using monads(list monad)

-- Problem 16 (Drop every Nth element)

-- Iterate along by 1 each time, taking the first and returning either the element, or [] (when i divides by n):
problem16 :: [a] -> Int -> [a]
problem16 [] _ = []
problem16 (x:xs) n = problem16' (x:xs) n 1 where
    problem16' (x:xs) n i = (if (n `mod` i == 0) then [] else[x]) ++ (problem16' xs n (i+1))
    problem16' [] _ _ = []

-- alternatively (List comprehension -> my idea)
problem16'' :: [a] -> Int -> [a]
problem16'' [] _ = []
problem16'' xs n = [xs!!(x - 1) | x <- [1..(length xs - 1)], x `mod` n /= 0]

-- Problem 17 (split the list into two parts, the length is given)
problem17 :: [a] -> Int -> ([a],[a])
problem17 =  flip splitAt  -- flips the arguments around, then uses splitAt

-- Alternatively
problem17' :: [a] -> Int -> ([a],[a])
problem17' xs n = ([xs!!x | x <- [0..(n-1)]], [xs!!x | x <- [n..(length xs - 1)]])


--alternatively by recursion
problem17'' :: [a] -> Int -> ([a],[a])
problem17'' [] _ = ([],[])
problem17'' (x : xs) n 
    | n > 0 = (x : ys, zs)
    | otherwise = ([], x:xs)
    where (ys,zs) = problem17'' xs (n - 1)

-- Problem 18 (Take a slice from a list from ith to kth element inclusive)
problem18 :: [a] -> Int -> Int -> [a]
problem18 xs i k
    | i < 1 = problem18 xs 0 k
    | k > length xs  = problem18 xs i (length xs)
    | i == k = []
    | otherwise = [xs!!(x - 1) | x <- [i..k]]

-- Problem 19 (Rotate a list n elements to the left)
problem19 :: [a] -> Int -> [a]
problem19 xs n = let len = length xs in [xs!!((x + n) `mod` len) | x <- [0..(len - 1)]]

-- alternatively using take and drop
problem19' :: [a] -> Int -> [a]
problem19' xs n = drop (n) xs ++ take n xs

-- Problem 20 (Remove kth element)
problem20 :: Int -> [a] -> ([a], [a])
problem20 n xs
    | n > length xs = (xs, [])
    | n < 1 = ([], xs)
    | otherwise = (take (n - 1) xs, drop n xs)

-- alternatively recurively
problem20' :: (Eq a) => Int -> [a] -> [a]
problem20' n l@(x:xs)
    | n < 1 = []
    | n == 1 = xs
    | l == [] = []
    | otherwise =  [x] ++ problem20' (n - 1) xs


-- ====================
--LIST PROBLEMS (21-28):
-- ====================


-- Problem 21 (Insert an element at a given position)
problem21 :: a -> [a] -> Int -> [a]
problem21 x xs n 
    | n < 1 = [x] ++ xs
    | n > length xs - 1  = xs ++ [x]
    | otherwise = let z = n - 1 in take (z)  xs ++ [x] ++ drop z xs

-- Alternatively (using foldr to apply a concatenation function, which uses a provided index and the values to decide if the insertion should occur)
problem21' :: a -> [a] -> Int -> [a]
problem21' x xs n = foldr problem12concat [] $ zip [1..] xs  -- $ operator says! work out zip function first, then put that in concat through foldr
    where 
        problem12concat (index, lstelement) xs
            | index == n = [x] ++ [lstelement] ++xs -- Correct place, join with  ++ (list concatenation)
            | otherwise = [lstelement] ++ xs

problem21'' :: a -> [a] -> Int -> [a]
problem21'' x xs n = foldr problem12concat [] $ zip [1..] xs  -- $ operator says! work out zip function first, then put that in concat through foldr
    where 
        problem12concat (index, lstelement) xs
            | index == n = x:lstelement:xs -- Correct place, join with : (adding head to list)
            | otherwise = lstelement:xs


-- Problem 22 (Create a liost containing all the integers in a given range)
problem22 :: Int -> Int -> [Int]
problem22 x y
    | x > y = error "first argument must eb smaller than second"
    | otherwise =[x..y]

-- Problem 23 (Extract a number of randomly selected element s from a list)
problem23 :: [a] -> Int -> [a]
problem23 = random



-- Running problems
main :: IO()
main = do
    print(problem22 3 7)






