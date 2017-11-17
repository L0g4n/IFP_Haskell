module Main where

-- main method
main :: IO ()
main = return ()

listEvens :: Int -> Int -> [Int]
listEvens x y
    | y < x = []
    | otherwise = [y | y <- [y, y - 1 .. x], y `mod` 2 == 0]

type Triple = (Int, Int, Int)

pythagoreanTriples :: Int -> [Triple]
pythagoreanTriples n 
    | n < 0 = []
    | otherwise = [(a, b, c) | a <- [1 .. n], b <- [a .. n], c <- [b .. n], a^2 + b^2 == c^2]
