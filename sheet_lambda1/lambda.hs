-- Provides a basic implementation of the (untyped) Lambda Calculus
module Lambda where

import Data.List (union, delete)

-- Data type for indexed variables, e.g. "x1, x2, ..."
data LamVariable = Name String | Idx Int String
    deriving Eq

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
    deriving Eq

-- pretty printing of lambda expressions
instance Show LamExpr where
    show (Var x) = show x
    show (Abs x e) = "(λ" ++ show x ++ ". " ++ show e ++ ")"
    show (App e1 e2) = show e1 ++ " " ++ show e2

-- returns a list of free variables in a given lambda expression
freeVarsLam :: LamExpr -> [LamVariable]
freeVarsLam (Var x) = [x]
freeVarsLam (Abs x e) = delete x (freeVarsLam e) -- delete the bound variable x from the rest
freeVarsLam (App e1 e2) = freeVarsLam e1 `union` freeVarsLam e2

-- returns the list of all variables in a given lambda expression
allVarsLam :: LamExpr -> [LamVariable]
allVarsLam (Var x) = [x]
allVarsLam (Abs x e) = [x] `union` allVarsLam e
allVarsLam (App e1 e2) = allVarsLam e1 `union` allVarsLam e2
