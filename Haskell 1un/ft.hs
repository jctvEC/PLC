type Nome = String
type Idade = Int
data Pessoas = Pessoa Nome Idade

jose = Pessoa "José" 23
mary = Pessoa "Maria" 25

ehigual :: Pessoas -> Pessoas -> Bool
ehigual (Pessoa n1 i1) (Pessoa n2 i2) | i1 == i2 && n1 == n2 = True
                                      | otherwise = False

data Shape = Circle Float | Rectangle Float Float | Square Float

area :: Shape -> Float
area (Circle r) = pi*(r^2)
area (Rectangle a1 a2) = a1 * a2
area (Square a) = a^2

data Expr = Num Int | Add Expr Expr | Sub Expr Expr

eval :: Expr -> Int
eval (Num n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

showExpr :: Expr -> String
showExpr (Num n) = show (n)
showExpr (Add e1 e2) = "(" ++ (showExpr e1) ++ "+" ++ (showExpr e2) ++ ")"
showExpr (Sub e1 e2) = "(" ++ (showExpr e1) ++ "-" ++ (showExpr e2) ++ ")"


data List t = Nil | Cons t (List t) 
data Tree t = NilT | Node t (Tree t)  (Tree t)

toList :: List t -> [t]
toList Nil = []
toList (Cons h t) = h : toList t
