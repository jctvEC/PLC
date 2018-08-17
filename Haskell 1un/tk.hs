type Chave =[(Char,Char)]
type FuncaoChave = (Char -> Char)

rot13parcial :: Chave    -- troca 'a' por 'n', 'b' por 'o' etc.
rot13parcial = [('a','n'),('b','o'),('c','p'),('d','q'),('e','r'),('f','s'),
                ('g','t'),('h','u'),('i','v'),('j','w'),('k','x'),('l','y'), ('m','z')]

cipher :: Chave -> String -> String
cipher [] (c:cs) = c : cipher rot13parcial cs 
cipher _ [] = []
cipher ((a,b):as) (c:cs) | a == c = b : cipher ((a,b):as) cs -- a,b = lista com elementos a trocar,  c,d = string 
                         | otherwise = cipher as (c:cs) 

inverteChave :: Chave -> Chave
inverteChave [] = []
inverteChave ((a,b):as) = (b,a) : inverteChave as

trocaSoLetraL :: FuncaoChave
trocaSoLetraL 'l' = 'b'
trocaSoLetraL c = c

cipherf :: FuncaoChave -> String -> String
cipherf _ [] = []
cipherf f (a:as) = f (a) : cipherf as