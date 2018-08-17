import Data.List

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (a:as) | (length as) == 0 = True 
                | a > head as = False
                | otherwise = isSorted as

bubble :: [Int] -> [Int]
bubble [] = []
bubble (a:as) | isSorted (a:as) = (a:as)
              | otherwise = bubble ( bsort (a:as))

bsort :: [Int] -> [Int]
bsort [] = []
bsort [a] = [a]
bsort (a:b:as) | a > b = b : bsort (a:as)
               | otherwise = a : bsort (b:as)
