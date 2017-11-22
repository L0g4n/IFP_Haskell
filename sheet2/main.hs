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
