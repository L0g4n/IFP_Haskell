module Main where


-- main method
main :: IO ()
main = return ()

-- returns x if both values are the same
and2 :: Bool -> Bool -> Bool
and2 x y = if x == y 
    then x
    else False
    

-- returns the minimum
-- ourMin :: Int -> Int -> Int
-- ourMin x y = if x < y then x else y

-- second possibility with guards
-- guards are tested top down
-- `otherwise` takes no arguments and always returns true
-- ourMin x y | x < y = x
--           | x >= y = y 


ourfun2 :: Float -> Float -> Float
-- local definitions
ourfun2 x y = (local + 1) * (local + 2) where 
    local = (ix + iy) / 2 where 
        ix = x
        iy = y


    
-- `Ord a` = type context
-- `a` = type variable
-- a has to be of Type `Ord`, which supports ordering
ourMin :: Ord a => a -> a -> a    
ourMin x y | x < y = x
           | x >= y = y 

-- typedef Cash to a tuple of Ints
type Cash = (Int, Int)
-- adds two tuples of "Cash" (i.e. 1.50 + 2.50)
addNumber :: Cash -> Cash -> Cash
addNumber (x, x') (y, y') = (x + y + (z `div` 100), z `mod` 100)
        where z = x' + y'

-- student marks example
type Name = String
type Marks = (Int, Int, Int)
type Entry = (Name, Marks)
type Total = (Name, Int)

-- takes a student entry and calculates a final mark (additon of the marks)
getMarks :: Entry -> Total
getMarks (n, (m1, m2, m3)) = (n, result) 
    where result = m1 + m2 + m3