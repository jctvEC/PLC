data Pessoas = Pessoa Nome Idade
data Shape = Circle Float | Rectangle Float Float | Square Float
data Tree t = NilT | Node t (Tree t)  (Tree t)
data Expr = Num Int | Add Expr Expr | Sub Expr Expr
data Diretorio = Arquivo Nome Tamanho | Pasta Nome [Diretorio]


type Tamanho = Int
type Nome = String
type Idade = Int
type Ponto = (Float,Float,Float)

arq1 = Arquivo "a1" 1
arq2 = Arquivo "a2" 2
arq3 = Arquivo "a3" 3

p1 = Pasta "P1" [arq1,p2]
p2 = Pasta "P2" [arq2,arq3]

jose = Pessoa "José" 23
mary = Pessoa "Maria" 25

ehigual :: Pessoas -> Pessoas -> Bool
ehigual (Pessoa n1 i1) (Pessoa n2 i2) | i1 == i2 && n1 == n2 = True
                                      | otherwise = False

area :: Shape -> Float
area (Circle r) = pi*(r^2)
area (Rectangle a1 a2) = a1 * a2
area (Square a) = a^2

eval :: Expr -> Int
eval (Num n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

showExpr :: Expr -> String
showExpr (Num n) = show (n)
showExpr (Add e1 e2) = "(" ++ (showExpr e1) ++ "+" ++ (showExpr e2) ++ ")"
showExpr (Sub e1 e2) = "(" ++ (showExpr e1) ++ "-" ++ (showExpr e2) ++ ")"

toList :: List t -> [t]
toList Nil = []
toList (Cons h t) = h : toList t

distancia :: Ponto -> Ponto -> Float
distancia (x1,y1,z1) (x2,y2,z2) = sqrt ((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)

size :: Diretorio -> (Tamanho, Int)
size (Arquivo n t) = (t, 1) 
size (Pasta np []) = (0,0)
size (Pasta np (a:as)) = somaTuplas (size a ) (size (Pasta "" as)) 

somaTuplas :: (Tamanho, Int) -> (Tamanho, Int) -> (Tamanho, Int)
somaTuplas  (n1,t1) (n2,t2) = (n1+n2,t1+t2)


data List t = Nil | Cons t (List t) 

-- L3
-- Apenas Funções recursivas
data Pilha1 t = Void | Elem t (Pilha1 t) deriving Show
-- Apenas lista
data Pilha2 t = Null | Casa t [Pilha2 t] deriving Show

push1 :: Pilha1 t -> Pilha1 t 
push1 Void = Pilha1 Elem t
push1 (Elem h t) = h : push1 t

pop1 ::  Pilha1 t -> [t]
pop1 Void = error "Pilha vazia!"
pop1 (Elem h t) = remove t

remove :: Pilha1 t -> [t]
remove Void = []
remove (Elem h t) = h : remove t 

top1 ::  Pilha1 t -> t
top1 Void = error "Pilha vazia!"
top1 (Elem h t) = h 

--nu = Void []
--e1 = Elem 1 (nu)


type Lista t = [t]
type Tupla x y = (x, y)

cabeca :: Lista t -> t
cabeca [ ] = error "Pilha vazia!"
cabeca (x:xs) = x

inverterTupla :: Tupla a b -> Tupla b a
inverterTupla (x, y) = (y, x)


data Expre = Literal Int -- um número
| Soma Expre Expre -- soma as duas expressões
| Subtrai Expre Expre -- subtrai a segunda expressão da primeira
| Variavel String -- representa uma variável, como ŭxŮ ou ŭyŮ
avaliar :: [(String, Int)] -> Expre -> Int
avaliar _ (Literal n) = n
avaliar [(a,b):bs] (Variavel n) = pegavar [(a,b):bs] n 
avaliar [(a,b):bs] (Soma e1 e2) = avaliar (e1) + avaliar (e2) 
avaliar [(a,b):bs] (Subtrai e1 e2 ) = avaliar (e1) - avaliar (e2)

pegavar :: [(String, Int)] -> String -> Int
pegavar [(a,b):bs]  str | str == b = a
                        | otherwise = pegavar bs str

eval :: Expr -> Int
eval (Num n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

--Exemplo:
-- expressão (x + (y - 33)) + (22 + x)
expressao = Soma (Soma (Variavel "x") (Subtrai (Variavel "y") (Literal 33))) (Soma
(Literal 22) (Variavel "x"))

-- (avaliar [("x", 10), ("y", 20)] expressao) = 29
