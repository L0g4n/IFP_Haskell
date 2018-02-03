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

-- Lambda expressions are simple variables,
-- applications of expressions to other expressions or abstractions, which represent functions with parameters and a body
data LamExpr = Var LamVariable
             | Abs LamVariable LamExpr
             | App LamExpr LamExpr
    -- deriving Eq

-- pretty printing of lambda expressions
instance Show LamExpr where
    show (Var x) = show x
    show (Abs x e) = "(λ" ++ show x ++ ". " ++ show e ++ ")"
    show (App e1 e2) = show e1 ++ " " ++ show e2
