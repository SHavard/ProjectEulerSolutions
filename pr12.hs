import Data.List

factors :: Int -> [Int]
factors x = nub $ 1 : x : [f' | f <- [2..ceiling $ sqrt $ fromIntegral x] , x `mod` f == 0 , f' <- [f, x `div` f]]

triangle :: Int -> Int
triangle x = sum [1..x]

problem = head [t | i <- [1..] , let t = triangle i , length (factors t) >= 500]