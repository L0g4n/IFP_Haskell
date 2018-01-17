module Lambda where

import Data.List (union, delete)
import Prelude hiding (id)

data Var = Name String
    deriving Eq

instance Show Var where
    show (Name x) = x

data Expr = LVar Var
          | LAbs Var Expr
          | LApp Expr Expr

instance Show Expr where
    show (LVar x) = show x
    show (LAbs x t) = "(\\" ++ show x ++ ". " ++ show t ++ ")"
    show (LApp t1 t2) = show t1 ++ " " ++ show t2

allVars :: Expr -> [Var]
allVars (LVar x) = [x]
allVars (LAbs x t) = union [x] (allVars t)
allVars (LApp t1 t2) = union (allVars t1) (allVars t2)

freeVars :: Expr -> [Var]
freeVars (LVar x) = [x]
freeVars (LAbs x t) = delete x (freeVars t)
freeVars (LApp t1 t2) = union (freeVars t1) (freeVars t2)


x :: Var
x = Name "x"

y :: Var
y = Name "y"

id :: Expr
id = LAbs x (LVar x)

t1 :: Expr
t1 = LApp id (LVar y)

dup :: Expr
dup = LAbs x (LApp (LVar x) (LVar x))

omega :: Expr
omega = LApp dup dup
