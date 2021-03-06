module Main where

-- main method
main :: IO ()
main = return ()

-- returns a list containing all even numbers between x and y in decreasing order
listEvens :: Int -> Int -> [Int]
listEvens x y
    | y < x = []
    | otherwise = [z | z <- [y, y - 1 .. x], z `mod` 2 == 0]


type Triple = (Int, Int, Int)

-- returns the list of pythagorean triples smaller than n
-- i.e. (a, b, c) with a <= b <= c and a²+b²=c²
pythagoreanTriples :: Int -> [Triple]
pythagoreanTriples n 
    | n < 0 = []
    | otherwise = [(a, b, c) | a <- [1 .. n], b <- [a .. n], c <- [b .. n], a ^ (2 :: Int) + b ^ (2 :: Int) == c ^ (2 :: Int)]

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

together :: [a] -> [(a, a)]
together xs 
    | length xs < 2 = []
    | otherwise = zip xs $ tail xs

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
nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth n (x : xs)
    | n == 0 = Just x
    | otherwise = nth (n - 1) xs

-- 2.2.3
-- remove all occurences of an item in a list

-- first with pattern-matching and guards
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove n (x : xs)
    | x == n = [] ++ rest
    | otherwise = x : rest
        where rest = remove n xs

-- second with `filter`
-- easier to write, because you do not have to implement the removal of one element, just the predicate.
remove2 :: Eq a => a -> [a] -> [a]
remove2 x xs = filter (\e -> e /= x) xs

-- 2.2.4
-- substitute all occurrences of a given item for another

-- first with recursion
substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute this that (x : xs)
    | x == this = that : rest
    | otherwise = x : rest
        where rest = substitute this that xs -- after we replaced `this` with `that` (or there was nothing to replace) we still have to loop through the rest of the list

-- second with `map`
substitute2 :: Eq a => a -> a -> [a] -> [a]
substitute2 this that = map (\x -> if x == this then that else x)


-- Exercise 2.3
-- higher-order functions

-- 2.3.1
addDashes :: [String] -> [String]
addDashes = map ("--" ++)

-- 2.3.2
swapPairs :: [(a, a)] -> [(a, a)]
swapPairs = map (\(x, y) -> (y, x))

-- with list comprehension
swapPairs2 :: [(a, a)] -> [(a, a)]
swapPairs2 xs = [(y, x) | (x, y) <- xs]

-- 2.3.3
applyEach :: [(a -> b, a)] -> [b]
applyEach xs = map (\(x, y) -> x y) xs

applyEach2 :: [(a -> b, a)] -> [b]
applyEach2 xs = [x y | (x, y) <- xs]

-- 2.3.4
theSame:: String -> Bool
theSame "" = True
theSame (c : cs) = all (== c) cs

-- 2.4.1
twice :: (a -> a) -> a -> a
twice f n = f(f n)

-- 2.4.2
iter :: Int -> (a -> a) -> a -> a
iter n f
    | n <= 0    = id
    | otherwise = f . iter (n-1) f

-- 2.4.3
mySum' :: Num a => [a] -> a
mySum' [] = 0
mySum' (a : as) =  a + mySum' as

mySum :: Num a => [a] -> a
mySum = foldr (+) 0

-- 2.4.4
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip = foldr (\(a, b) (as, bs) -> (a : as, b : bs)) ([], [])

-- 2.4.5
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs
