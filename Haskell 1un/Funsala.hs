fat :: Integer -> Integer
fat n | n == 0 = 1 
      | n > 0 = n*fat (n-1) 
      | otherwise = error "Fatorial de num negativo"



primo :: Int -> Bool
primo x = incremento x 2

incremento :: Int -> Int -> Bool
incremento x y | (y < x) && (x `mod` y == 0) = False
incremento x y | x == y || x == 1 = True
               | otherwise = incremento x (y+1)


divisaoeuclidiana :: Int -> Int -> (Int,Int)
divisaoeuclidiana x y | x < y =  (0,x)  
                      | otherwise = contador x y 0

contador :: Int -> Int -> Int -> (Int,Int)
contador x y z | x < y = (z,x)
               | otherwise = contador (x-y) y (z+1)


{-sumSquares x y = sqX + sqY  -- Com where, "Usa" depois define
           where sqX = x * x
                 sqY = y * y-}

sumSquares x y = let sqX = x * x -- Com let in, define, depois usa
                     sqY = y * y
                 in  sqX + sqY 

addEspacos :: Int -> String
addEspacos n | n == 0 = ""
             | n > 0 = " " ++ addEspacos(n-1) 

paraDireita :: Int -> String -> String
paraDireita n x = addEspacos n ++ x 

sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as

listaDeNumerosEntre :: Int -> Int -> [Int]
listaDeNumerosEntre x y | x > y = []
                        | otherwise = x : listaDeNumerosEntre (x+1) y

listaEntre3 :: Int -> Int -> Int -> [Int]
listaEntre3 a b c | (a > b) && (b < c) = []
                  | (a < b) && (c < a) = []
                  | otherwise = a : listaEntre3 b ( b + (b-a)) c


dobra :: [Int] -> [Int]
dobra [] = []
dobra  (a:as) = 2 * a : dobra (as)

member :: [Int] -> Int -> Bool
member [] x = False
member (a:as) x | a == x = True
                | otherwise = member as x

charmember :: [Char] -> Char -> Bool
charmember [] x = False
charmember (a:as) x | a == x = True
                    | otherwise = charmember as x

digits :: String -> String
digits [] = []
digits (a:as) | a == '1' || a == '2' || a == '3' || a == '4' || a == '5' || a == '6' || a == '7' || a == '8' || a == '9' = a : digits as -- a >= '0' && a <= '9'
              | otherwise = digits as

sumPairs :: [(Int,Int)] -> [Int]
sumPairs [] = []
sumPairs ((a,b):abs) = a + b : sumPairs abs


-- Polimorfismo

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]


baseExemplo :: BancoDados
baseExemplo = [("Damurie","Paga a carona Thyago"),("Nonon","Como ser monitor de EC"),("Douglinhas","O Apressado Maci"),("Luqueta","Minha prima e um fenomeno")]
-- Livros Emprestados

{-livros :: BancoDados -> Pessoa -> [Livro]
livros [] p = []
livros ((pbd,lbd):pbdp) p | p == pbd = lbd : livros pbdp p
                          | otherwise = livros pbdp p
-}
livros :: BancoDados -> Pessoa -> [Livro]
livros bd p = [ lbd | (pbd,lbd) <- bd , p == pbd]

{-emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] l = [] 
emprestimos ((pbd,lbd):pbdp) l | l == lbd = pbd : emprestimos pbdp l
                               | otherwise = emprestimos pbdp l -}

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd l = [ pbd | (pbd,lbd) <- bd, l == lbd ]


emprestado :: BancoDados -> Livro -> Bool
emprestado [] l = False
emprestado ((pbd,lbd):pbdp) l | l == lbd = True
                              | otherwise = emprestado pbdp l

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] p = 0
qtdEmprestimos ((pbd,lbd):pbdp) p | p == pbd  = 1 + qtdEmprestimos pbdp p
                                  | otherwise = qtdEmprestimos pbdp p

-- Atualizações

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar bd p l = (p,l) : bd

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] p l = []
devolver ((pbd,lbd): pbdp) p l | (p == pbd) && (l == lbd) = pbdp
                               | otherwise = (pbd,lbd) : devolver pbdp p lbd
{- não tá funfando
devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver bd p l [(pbd,lbd) | (pbd,lbd) <- bd, (p,l) /= (pbd,lbd)]
-}

doubleList xs = [2*a| a <- xs]

--quicksort
qs :: [Int] -> [Int]
qs [] = []
qs (a:as) = qs [x | x <- as, x < a] ++ [a] ++ qs [ y | y <- as, y >= a]


{- exemplo de funcionamento do quick
qs[9,25,3,12,7,0]
qs[3,7,0] ++ [9] ++ qs[25,12]
0 ++ [3] ++ [7] ++ [12] ++ [25] ++ []
-}

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x as = [ y | y <- as , y <= x] ++ [x] ++ [ z | z <- as, z < x]

{- 
insort :: Int -> [Int] -> [Int]
insort x [] = [x]
insort x as = insort[ y | y <- as , y <= x] ++ [x] ++ insort[ z | z <- as, z > x]
-}

palindromo :: String -> Bool
palindromo str | str == reverse str = True
               | otherwise = False 

{-
inverte :: [a] -> [a]
inverte [] = []
inverte (a:as) | (inverte as) ++ [b] 
-}


doisAdois :: Int -> [(Int,Int)]
doisAdois x = [(a,b) |  a <-[0..x],b <-[0..x] ,a <= b]
baixa [("Palio", 1999, 15000), ("Gol", 2003, 18000), ("Kombi", 2005,17000), ("Palio", 2001, 18000), ("Celta", 2004, 12000), ("Corsa", 2000, 13000),("Saveiro", 1999, 14500), ("Gol", 2008, 18000)] 2001