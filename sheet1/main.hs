module Main where
import Data.Char (toUpper)
import Data.List (isInfixOf)

-- main method
main :: IO ()
main = return ()
 
-- returns 2*x
double :: Int -> Int
double x = 2 * x

-- takes two floats as arguments and returns the ratio
ratio :: Float -> Float -> Float
ratio x y = (x + y) / (x - y)

-- returns the length of the hypotenuse according to the pythagorean theorem
hypotenuse :: Double -> Double -> Double
hypotenuse x y = sqrt (x^2 + y^2)

xIntercept :: Double -> Double -> Double
xIntercept m c = (-c) / m

--- returns tree if all arguments are pairwise different
threeDiff :: Integer -> Integer -> Integer -> Bool
threeDiff m n p = if m /= n && m /= p && p /= n 
    then True
    else False
    
-- returns the average of three Integers, convert to Float with `fromIntegral`
averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

-- computes the value of an arithmetic series
arithmeticSum :: Int -> Int -> Int -> Int
arithmeticSum a n d = n * (2 * a + (n-1) * d) `div` 2

-- returns true if `x` lies between a and b or b and a
inrange1 :: Double -> Double -> Double -> Bool
inrange1 x a b = if x > a && x < b || x > b && x < a 
    then True
    else False

-- computes the "exclusive or" of two bools
orExclusive :: Bool -> Bool -> Bool
orExclusive x y | x && not y = x
                | y && not x = y
                | otherwise = False
    
-- computes the logical implication of two bools
implies :: Bool -> Bool -> Bool
implies x y | not x && y = False
            | otherwise  = True
    
-- returns the hundred quantity of an integer
hundreds :: Int -> Int
hundreds x = x `mod` 1000 `div` 100

-- returns the middle character of a non-empy string
middle :: String -> Char
middle str | length str > 0 = str !! div (length str) 2
           | otherwise = '\0'

-- type: Char -> Char
-- the function to use is `toUpper` from the `Data.Char`
toUppercase :: Char -> Char
toUppercase c = Data.Char.toUpper c

-- type: String -> String -> Bool
-- the function to use is `isInfixOf` 
isSubstring :: String -> String -> Bool
isSubstring substr str = Data.List.isInfixOf substr str

-- returns the nth number in the sequence of natural numbers
natural :: Int -> Int
natural x | x <= 1 = 0
          | x == 2 = 1
          | x == 3 = 2
          | x == 4 = 3
          | otherwise = natural (x-1) + natural 2

sumRec :: Int -> Int
sumRec n | n <= 0 = 0
         | otherwise = n + sumRec (n - 1)

factorial :: Int -> Int
factorial n | n <= 0 = 1
            | otherwise = n * factorial (n-1)

sumFact :: Int -> Int
sumFact n | n < 0 = 0
          | otherwise = factorial n + sumFact (n-1) 
        

-- TODO: return the nth number in the arithmetic series `a, a+d,a+2*d, ...`
arithmeticSeries :: Int -> Int -> Int -> Int
arithmeticSeries a n d | n <= 0 = a
                       | otherwise = 12

type Time = (Int, Int)

-- adds two tuples of `Time`
addTimes :: Time -> Time -> Time
addTimes (x, x') (y, y') = ((x + y) + carry, (x' + y') `mod` 60) where
    carry = (x' + y') `div` 60

{- --TODO: this version does not work currently. 
-- recursive function to sum over a list of `Time`s, i.e. `[Time]`
sumTimes :: [Time] -> Time
sumTimes [] = (0, 0)
sumTimes (x : xs) = addTimes x (0, 0) + sumTimes xs -}

-- sum a list of `Time`s with an accumulator
sumTimes :: [Time] -> Time -> Time
sumTimes [] y = addTimes y (0, 0)
sumTimes (x : xs) y = sumTimes xs (addTimes x y)

type Moves = (Char, Char)
type Solution = [Moves]

hanoi :: Int -> Solution
hanoi n = hanoi' n 'a' 'c' 'b'

hanoi' :: Int -> Char -> Char -> Char -> Solution
hanoi' n a c b
    | n <= 0 = []
    | otherwise = hanoi' (n-1) a b c ++ (a, c) : hanoi' (n-1) b c a
