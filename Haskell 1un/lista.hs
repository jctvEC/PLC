import Data.List


tuplaQuant :: [Int] -> [(Int,Int)]
tuplaQuant  z = nub [ (x,y) | x <- z, y <- [0..999]  , y == length ( elemIndices x (sort z)) ]


reduz1 :: [Int] -> [Int]
reduz1 z = [x | x <- (z \\ nub z)]