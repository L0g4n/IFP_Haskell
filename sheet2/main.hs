module Main where

-- main method
main :: IO ()
main = return ()

listEvens :: Int -> Int -> [Int]
listEvens x y
    | y < x = []
    | otherwise = [y | y <- [y, y - 1 .. x], y `mod` 2 == 0]

