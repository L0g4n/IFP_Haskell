module Main where

-- main method
main :: IO ()
main = return ()

-- 3.1.1
data Season = Spring | Summer | Autumn | Winter
        deriving (Eq, Show, Enum)

theSeasons :: [Season]
theSeasons = enumFrom Spring

-- 3.1.2
seasonsFrom :: Season -> [Season]
seasonsFrom input = enumFromTo input Winter

-- 3.1.3
mapSeasonsFrom :: [Season] -> [[Season]]
mapSeasonsFrom xs = map (\x -> [] ++ seasonsFrom x) xs

-- 3.1.4
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec 
        deriving (Eq, Ord, Show, Enum)

intMonth :: Month -> Int
intMonth s = fromEnum s + 1

intMonthToEnum :: Int -> Month
intMonthToEnum n = toEnum (n - 1)

-- 3.1.5
monthFromTo :: Month -> Month -> [Month]
monthFromTo start end = [start .. end]

-- 3.1.6
monthToSeason :: Month -> Season
monthToSeason month = toEnum $ (fromEnum month - 2) `div` 3 `mod` 4

-- 3.1.7
data MyBoolean = MyFalse | MyTrue
        deriving (Show, Eq)

booltoMyBoolean :: Bool -> MyBoolean
booltoMyBoolean b 
    | b == False = MyFalse
    | otherwise = MyTrue

myBooleanToBool :: MyBoolean -> Bool
myBooleanToBool MyTrue = True
myBooleanToBool MyFalse = False

-- 3.1.8
(&:&) :: MyBoolean -> MyBoolean -> MyBoolean
(&:&) (MyTrue) (MyTrue) = MyTrue
(&:&) _ _ = MyFalse

(|:|) :: MyBoolean -> MyBoolean -> MyBoolean
(|:|) (MyFalse) (MyFalse) = MyFalse
(|:|) _ _ = MyTrue

-- 3.1.9
-- recursively
myAnd :: [MyBoolean] -> MyBoolean
myAnd [] = MyTrue
myAnd (MyFalse : _) = MyFalse
myAnd (_ : bs) = myAnd bs

-- TODO:
{- -- with `map`
myAnd' :: [MyBoolean] -> MyBoolean
myAnd' bs = map -}

-- with `foldr`
myAnd'' :: [MyBoolean] -> MyBoolean
myAnd'' mybs = foldr (&:&) MyTrue mybs

-- recursively
myOr :: [MyBoolean] -> MyBoolean
myOr [] = MyFalse
myOr (MyTrue : _) = MyTrue
myOr (_ : bs) = myOr bs

-- with `foldr`
myOr'' :: [MyBoolean] -> MyBoolean
myOr'' mybs = foldr (|:|) MyFalse mybs

-- 3.1.10
data Bit = O | I
        deriving (Enum)

bitsToInt :: [Bit] -> Int
bitsToInt bits = bitsToIntHelper bits acc
                    where acc = length bits - 1

-- with accumulator
bitsToIntHelper :: [Bit] -> Int -> Int
bitsToIntHelper [] _ = 0
bitsToIntHelper (bit : bits) acc = 2^acc * fromEnum bit + bitsToIntHelper bits (acc - 1)

{- 
=============
Exercise 3.2
=============
-}

-- 3.2.1
data Number = Exact Int | Approx Float

rounded :: Number -> Int
rounded (Exact n) = n
rounded (Approx f) = round f

-- 3.2.2
data Age = Years Int 
        deriving Show

data Name = Name String String
        deriving Show

data Person = Person Name Age
        deriving Show

firstName :: Person -> String
firstName (Person (Name first _) _) = first

howOld :: Person -> Age
howOld (Person _ age) = age

addAges :: Person -> Person -> Age
addAges (Person _ (Years age)) (Person _ (Years age')) = sumAge
    where sumAge = (Years (age + age'))

-- 3.2.3
-- Circle has a radius, Rectange has width & height
data Shape = Circle Float | Rectangle Float Float
        deriving Show

-- 3.2.4
isRound :: Shape -> MyBoolean
isRound (Circle _) = MyTrue
isRound _ = MyFalse

-- 3.2.5
getArea :: Shape -> Float
getArea (Circle radius) = pi * radius ** 2
getArea (Rectangle l1 l2) = l1 * l2

-- 3.2.6
data Point = Point Float Float
        deriving Show

dist :: Point -> Point -> Float
dist (Point x1 y1) (Point x2 y2) = result
    where result = sqrt((y2 - y1) ** 2 + (x2 - x1) ** 2)

-- 3.2.7
data Slope = Value Float | Infinite
        deriving Show

getSlope :: Point -> Point -> Slope
getSlope (Point x1 y1) (Point x2 y2) = m
    where m = if x1 == x2 && y2 == y1
              then Infinite
              else (Value (diffy / diffx))
                where diffy = y2 - y1
                      diffx = x2 - x1


-- 3.2.8
data YIntercept = Intercept Float | Undefined
                deriving Show

getYIntercept :: Point -> Slope -> YIntercept
getYIntercept (Point x y) (Value m) = b
        where b = (Intercept (y - m * x))
getYIntercept _ _ = Undefined

-- 3.2.9
-- the `Point` is the center of the figure
-- no orientation of an object, so assume that all rectangles lie with the longest of its two sides parallel to the x-axis
-- the other side parallel to the y-axis, if two sides have the same length, it lies with its sides parallel to the axes
data Figure = Figure Shape Point
        deriving Show

-- moves a `Figure` by the two offsets given
move :: Float -> Float -> Figure -> Figure
move x_off y_off (Figure shape (Point x y)) = transl
        where transl = (Figure shape (Point (x + x_off) (y + y_off)))

-- 3.2.10
-- tests whether two figures overlap, i.e. two circles, two rectangles or a rectangle and a circle
overlap :: Figure -> Figure -> Bool
overlap (Figure (Circle r1) p1) (Figure (Circle r2) p2) = cond
        where cond = dist p1 p2 < r1 + r2

-- determines whether the first rectangle is completely to the left of the second one
-- thats the case when all points of the first have a smaller x-coordinate than the smallest x-coordinate of the second
leftOf :: Figure -> Figure -> Bool
leftOf (Figure (Rectangle w1 _) (Point x1 _)) (Figure (Rectangle _ _) (Point x2 _)) = cond
        where cond = trc_x < x2
              trc_x = x_right
              x_right = x1 + w1

-- checks if first rectangle is right of the second one
rightOf :: Figure -> Figure -> Bool
rightOf (Figure (Rectangle _ _) (Point x1 _)) (Figure (Rectangle w2 _) (Point x2 _)) = cond
        where cond = x1 > trc_x
              trc_x = x_right
              x_right = x2 + w2

-- checks if first rectangle is above the second one
above :: Figure -> Figure -> Bool
above (Figure (Rectangle _ h1) (Point _ y1)) (Figure (Rectangle _ _) (Point _ y2)) = cond
        where cond = tlc_y < y2
              tlc_y = y1 + h1
