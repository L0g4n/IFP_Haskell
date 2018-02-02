-- Provides a basic implementation of the Lambda Calculus
module Lambda where

-- Data type for indexed variables, e.g. "x1, x2, ..."
data LamVariable = Name String | Idx Int String

-- instance for show to provide "fancy printing" of variables
instance Show LamVariable where
    show (Name x) = x
    show (Idx i x)
        | i == 0 = x -- Index 0 is always omitted
        | otherwise = x ++ show i
