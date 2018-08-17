type Nome = String
type Ano = Int
type Preco = Int
--4º
count :: Int -> [Int] -> Int  --a)
count _ [] = 0
count x (a:as)| x == a = 1 + count x as
              | otherwise = count x as

countcl :: Int -> [Int] -> Int  --a)
countcl _ [] = 0
countcl x l = length ([z | z <- l,  z == x ])
 
check :: Int -> [Int] -> Bool --b)
check _ [] = False
check x (a:as)| x == a = True
              | otherwise = check x as

checkcl :: Int -> [Int] -> Bool --b)
checkcl x l | (count x l) > 0 = True
            | otherwise = False

--fromTocl :: Int -> Int -> Int -> [Int]
--fromTocl a b c | length [l | l <- [a] , ((a < b && c < b && c > 0) || (a>b && c < 0))] == 0 = []
  --             | length [l | l <- [a] , ((a < b && c < b && c > 0) || (a>b && c < 0))] == 0 = []

fromTo :: Int -> Int -> Int -> [Int]
fromTo a b c| ((a < b && c < b && c > 0) || (a>=b && c < 0)) =  [a] ++ fromTo (a+c) b c
            | otherwise = []

baixa:: [(String, Int, Int)] -> Int -> [(String, Int, Int)]
baixa [] _ = []
baixa cbd al = [(x,y,z)| (x,y,z) <- cbd,  y < al]
--baixa [("Palio", 1999, 15000), ("Gol", 2003, 18000), ("Kombi", 2005,17000), ("Palio", 2001, 18000), ("Celta", 2004, 12000), ("Corsa", 2000, 13000),("Saveiro", 1999, 14500), ("Gol", 2008, 18000)] 2001

apurado ::  [(String, Int, Int)] -> Int -> (Int, Int)
apurado [] _ = (0,0)
apurado cbd al =  (length (baixa cbd al),somat (baixa cbd al))

--apurado [("Palio", 1999, 15000), ("Gol", 2003, 18000), ("Kombi", 2005,17000), ("Palio", 2001, 18000), ("Celta", 2004, 12000), ("Corsa", 2000, 13000),("Saveiro", 1999, 14500), ("Gol", 2008, 18000)] 2001

somat :: [(String, Int, Int)] -> Int 
somat [] = 0
somat ((n,a,p):as) = p + somat as 

criaDomino :: Int -> [(Int,Int)]
criaDomino x = [(a,b) |  a <-[0..x],b <-[0..x] ,a <= b]

divisaoEuclidiana ::Int -> Int -> (Int,Int)
divisaoEuclidiana a b | a < b = (0,a)
                      | otherwise  = auxdiv a b 0 -- quebra com valores negativos em b

auxdiv :: Int -> Int -> Int -> (Int,Int)
auxdiv a b i | a < b = (i,a)
             | b < 0 = auxdiv (a+b) b (i-1) 
             | otherwise = auxdiv (a-b) b  (i+1)

--nPrimo :: Int -> Int
--nPrimo 0 = 0 -- n existe
--nPrimo 1 = 2
--nPrimo = incremento x 2 

--aux2 :: Int ->  Int-> (Int,Int) -> Int
--aux2 x y |  == x = 1
--         | incremento x+1 2 x

--incremento :: Int -> Int -> Int -> ( Int,Int)
--incremento x y z  | (y < x) && (x `mod` y == 0) = incremento (x+1) 2 z
--incremento x y z  | x == y = (x,z+1) 
--                  | otherwise = incremento x (y+1) z

ordenaPeso :: (a -> Int) -> [a] -> [a]
ordenaPeso f []= []
ordenaPeso f (a:as) = ordenaPeso f [y |y<-as ,(f y) <= (f a)] ++ [a] ++ ordenaPeso f [y |y<-as,(f y) >= (f a)]
--
maiorSeqCollatz :: [Int] -> Int
maiorSeqCollatz [] = 0
maiorSeqCollatz x = foldr1 maxi (map verificacollatz (filter (>0) x))

maxi :: Int -> Int -> Int
maxi a b | a > b = a
maxi a b | otherwise = b

verificacollatz :: Int -> Int
verificacollatz x | x == 1 = 1
                  | mod x 2 == 0 = 1 + verificacollatz (div x 2)
                  | otherwise = 1 + verificacollatz ((3*x)+1)

--aplicaFuncoes :: [a -> a] -> [a] -> [a]
--aplicaFuncoes _ [] = []
--aplicaFuncoes f (a:as) = apfun f [a] ++ apfun f as

--apfun :: [a->a] -> [a] -> [a]
--apfun [] x = x
--apfun (f:fs) x = apfun fs (f x) 

--aplicaFuncoes f (a:as) = aplicaFuncoes f [y |y<-as ,map (f y) <= (f a)] ++ [a] ++ ordenaPeso f [y |y<-as,(f y) >= (f a)]


data Expre = Literal Int | Soma Expre Expre | Subtrai Expre Expre | Variavel String

avaliar :: [(String, Int)] -> Expre -> Int
avaliar _ (Literal n) = n
avaliar x (Variavel n) = pegavar x n
avaliar x (Soma e1 e2) = avaliar x (e1) + avaliar x (e2) 
avaliar x (Subtrai e1 e2 ) = avaliar x (e1) - avaliar x (e2)

pegavar :: [(String, Int)] -> String -> Int
pegavar ((a,b):bs)  str | str == a = b
                        | otherwise = pegavar bs str

--Exemplo:
-- expressão (x + (y - 33)) + (22 + x)
expressao = Soma (Soma (Variavel "x") (Subtrai (Variavel "y") (Literal 33))) (Soma(Literal 22) (Variavel "x"))

--(avaliar [("x", 10), ("y", 20)] expressao) = 29


data KeyTree = Empty | Node Char Char KeyTree KeyTree

chaveParcial :: KeyTree
chaveParcial = Node 'h' 'u' (Node 'c' 'p' (Node 'b' 'o' (Node 'a' 'n' Empty Empty)Empty) (Node 'e' 'r' Empty Empty)) (Node 'l' 'y' Empty (Node 'm' 'z' Empty Empty))

cipherT :: KeyTree -> String -> String
cipherT _ [] = []
cipherT x (c:cs) = cipheraux x c : cipherT x cs

cipheraux :: KeyTree -> Char -> Char
cipheraux (Empty) c = c
cipheraux (Node a b ne nd) c | c == a = b
                             | c < a = cipheraux ne c
                             | otherwise = cipheraux nd c
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (a:as) | (length as) == 0 = True 
                | a > head as = False
                | otherwise = isSorted as

bubble :: [Int] -> [Int]
bubble [] = []
bubble (a:as) | isSorted (a:as)= (a:as)
              | otherwise = bubble (bSort (a:as))

bSort :: [Int] -> [Int]
bSort [] = []
bSort [a] = [a]
bSort (a:b:as) | a < b = a : bSort (b:as)
               | otherwise = b : bSort (a:as)


logSetembro = "2016-09-27;19:31:52;Normal;208772;\n2016-09-27;18:12:02;Normal;155759;\n2016-09-26;17:12:02;Normal;155759;\n2016-09-26;16:11:02;Denied;188758;\n2016-09-25;19:12:02;Normal;155759;"

tiposDeAcesso :: String -> (Int , Int) 
tiposDeAcesso [] = (0,0)
tiposDeAcesso (a:as) = somaTuplas (a:as)  (0,0)

somaTuplas :: String -> (Int,Int) -> (Int,Int) 
somaTuplas [] (n, d)= (n,d)
somaTuplas (a:as) (n,d) | a == 'N' = somaTuplas as ((n+1),d)
                        | a == 'D' = somaTuplas as (n,(d+1))