module Main where

-- main method
main :: IO ()
main = return ()

-- returns a list containing all even numbers between x and y in decreasing order
listEvens :: Int -> Int -> [Int]
listEvens x y
    | y < x = []
    | otherwise = [y | y <- [y, y - 1 .. x], y `mod` 2 == 0]


type Triple = (Int, Int, Int)

-- returns the list of pythagorean triples smaller than n
-- i.e. (a, b, c) with a <= b <= c and a²+b²=c²
pythagoreanTriples :: Int -> [Triple]
pythagoreanTriples n 
    | n < 0 = []
    | otherwise = [(a, b, c) | a <- [1 .. n], b <- [a .. n], c <- [b .. n], a^2 + b^2 == c^2]

-- returns a list containing the sum of the corresponding elements of the two lists, i.e. drops any element that ends up unpaired
-- e.g. `[1, 2] [3, 4, 5] == [4,6]`, `zipWith` basically does all the work
addPairwise :: [Int] -> [Int] -> [Int]
addPairwise x y = zipWith (+) x y

subList :: [a] -> (Int, Int) -> [a]
subList list (i, j)
    | i < 0 || j < i || j > length list - 1 = []
    | otherwise = [list !! n | n <- [i .. j]]

subList' :: [a] -> (Int, Int) -> [a]
subList' list (i, j)
    | i < 0 || j < i || j > length list - 1 = []
    | otherwise = drop i takeLs
        where takeLs = take (j+1) list

{- together :: [a] -> [(a, a)]
together xs 
    | length xs < 2 = []
    | otherwise = zip 1 xs $ tail -}

-- Exercise 2.2
-- all functions have to be **recursively** defined

-- 2.2.1
contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains n (x : xs) = if n == x then True else contains n xs

-- this time with higher-order functions and selections
contains2 :: Eq a => a -> [a] -> Bool
contains2 n xs = any (== n) xs

-- 2.2.2
-- returns the nth item in a list 
{- TODO:
nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth n xs
    | n > length xs || n < 0 = Nothing
    | n -}    

-- 2.2.3
-- remove all occurences of an item in a list

-- first with pattern-matching and guards
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y : ys) = same x y ++ remove x ys

same :: Eq a => a -> a -> [a]
same x y 
    | x == y = []
    | otherwise = [y]

-- second with `filter`
remove2 :: Eq a => a -> [a] -> [a]
remove2 x xs = filter (\e -> e /= x) xs
