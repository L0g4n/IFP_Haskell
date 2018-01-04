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

-- recursively
myOr :: [MyBoolean] -> MyBoolean
myOr [] = MyFalse
myOr (MyTrue : _) = MyTrue
myOr (_ : bs) = myOr bs
