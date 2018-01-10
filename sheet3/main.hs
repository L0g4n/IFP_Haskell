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
-- two circles
overlap (Figure (Circle r1) p1) (Figure (Circle r2) p2) = cond
        where cond = dist p1 p2 < r1 + r2
-- two rectangles
overlap (Figure (Rectangle w1 h1) p1) (Figure (Rectangle w2 h2) p2) = cond
        where cond = not (leftOf w1 p1 p2) && not (rightOf p1 w2 p2) && not (above h1 p1 p2) && not (below p1 h2 p2)
-- a circle and a rectangle
overlap (Figure (Circle r1) p1)  (Figure (Rectangle w h) p2) = cond
        where cond = circleInRect p1 w h p2 || rectCornerInCircle w h p1 r1 p1 || circleOverlapsSideRect r1 p1 w h p2

-- determines whether the first rectangle is completely to the left of the second one
-- thats the case when all points of the first have a smaller x-coordinate than the smallest x-coordinate of the second
leftOf :: Float -> Point -> Point -> Bool
leftOf w1 (Point x1 _) (Point x2 _) = cond
        where cond = trc_x < x2
              trc_x = x_right
              x_right = x1 + w1

-- checks if first rectangle is right of the second one
rightOf :: Point -> Float -> Point -> Bool
rightOf (Point x1 _) w2 (Point x2 _) = cond
        where cond = x1 > trc_x
              trc_x = x_right
              x_right = x2 + w2

-- checks if first rectangle is above the second one
above :: Float -> Point -> Point -> Bool
above h1 (Point _ y1) (Point _ y2) = cond
        where cond = tlc_y < y2
              tlc_y = y1 + h1

-- checks if first rectangle is below the second one
below :: Point -> Float -> Point -> Bool
below (Point _ y1) h2 (Point _ y2) = cond
        where cond = y1 > tlc_y
              tlc_y = y2 + h2

-- first case of a circle colliding with a rectangle
-- checks if center of circle is *in* the rectangle
circleInRect :: Point -> Float -> Float -> Point -> Bool
circleInRect (Point mx my) w h (Point x y) = cond
        where cond = my > y && my < tlc_y && mx > x && mx < trc_x
              tlc_y = y + h
              trc_x = x + w

-- second case
-- checks if a corner of a rectangle is *in* the circle
rectCornerInCircle :: Float -> Float -> Point -> Float -> Point -> Bool
rectCornerInCircle w h (Point x y) r m = cond
        where cond = dist tlc m < r || dist trc m < r || dist blc m < r || dist brc m < r
              tlc = (Point x y)
              trc = (Point (x + w) y)
              blc = (Point x (y + h))
              brc = (Point (x + w) (y + h))

-- third and last case
-- checks if a circle overlaps a side of a rectangle
circleOverlapsSideRect :: Float -> Point -> Float -> Float -> Point -> Bool
circleOverlapsSideRect r (Point mx my) w h (Point x y) = cond
        where cond = topside || leftside || bottomside || rightside
              topside = mx > tlc_x && mx < trc_x && dist m top < r
              tlc_x = x
              trc_x = x + w
              m = (Point mx my)
              top = (Point mx y)

              leftside = my > tlc_y && my < blc_y && dist m left < r
              tlc_y = y
              blc_y = y + h
              left = (Point x my)

              bottomside = mx > blc_x && mx < brc_x && dist m bottom < r
              blc_x = x
              brc_x = trc_x
              bottom = (Point mx blc_y)

              rightside = my > trc_y && my < brc_y && dist m right < r
              trc_y = y
              brc_y = blc_y
              right = (Point trc_x my)

{- 
=============
Exercise 3.3
=============
-}

-- recursive type
data Expr = Lit Int
          | OpExpr Op Expr Expr
    deriving Eq

-- 3.3.1
-- counts the number of operations in an expression
size :: Expr -> Int
size (Lit _) = 0
size (OpExpr _ e1 e2) = 1 + size e1 + size e2

-- 3.3.2
-- evaluates the expression
eval :: Expr -> Int
eval (Lit n) = n
eval (OpExpr Add e1 e2) = eval e1 + eval e2
eval (OpExpr Sub e1 e2) = eval e1 - eval e2
eval (OpExpr Mul e1 e2) = eval e1 * eval e2
eval (OpExpr Div e1 e2) = eval e1 `div` eval e2
eval (OpExpr Mod e1 e2) = eval e1 `mod` eval e2

-- 3.3.3
-- `Div` is integer division
data Op = Add | Sub | Mul | Div | Mod
    deriving Eq


-- Exercise 3.5
-- ============

-- 3.5.1
data Pair a = Pair a a
    deriving Show

swapPair :: Pair a -> Pair a
swapPair (Pair a b) = (Pair b a)

eqPair :: Eq a => Pair a -> Bool
eqPair (Pair a b) = a == b

-- 3.5.2
-- list is head : tail
data List a = EmptyList | Cons a (List a) 
        deriving (Eq, Ord, Show, Read)

isEmpty :: List a -> Bool
isEmpty EmptyList = True
isEmpty _ = False

lengthOfList :: List a -> Int
lengthOfList EmptyList = 0
lengthOfList (Cons _ xs) = 1 + lengthOfList xs

-- Exercise 3.6
-- ============

data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a) 
        deriving Show

-- 3.6.1
howMany :: Tree a -> Int
howMany Empty = 0
howMany (Leaf _) = 1
howMany (Node _ a b) = 1 + howMany a + howMany b

-- 3.6.2
depth :: Tree a -> Int
depth Empty = 0
depth (Leaf _) = 1
depth (Node _ t1 t2) = 1 + max (depth t1) (depth t2)

-- 3.6.3
reflect :: Tree a -> Tree a
reflect Empty = Empty
reflect (Leaf x) = (Leaf x)
reflect (Node x t1 t2) = Node x (reflect t2) (reflect t1)

-- 3.6.5
normalize :: Tree a -> Tree a
normalize Empty = Empty
normalize (Leaf x) = Leaf x
normalize (Node x Empty Empty) = Leaf x
normalize (Node x t1 t2) = Node x (normalize t1) (normalize t2)

-- 3.6.6
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node x t1 t2) = Node (f x) (mapTree f t1) (mapTree f t2)
