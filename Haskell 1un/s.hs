import Data.List

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (a:as) | (length as) == 0 = True 
                | a > head as = False
                | otherwise = isSorted as

bSort :: [Int] -> [Int]
bSort [] = []
bSort (a:as) | isSorted (a:as) =  (a:as)
             | otherwise = bSort ( bubble (a:as))

bubble :: [Int] -> [Int]
bubble [] = []
bubble [a] = [a]
bubble (a:b:as) | a > b = b : bubble (a:as)
                | otherwise = a : bubble (b:as)
