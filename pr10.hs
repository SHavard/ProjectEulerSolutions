import Data.List

factors :: Int -> [Int]
factors x = nub $ 1 : x : [f' | f <- [2..ceiling $ sqrt $ fromIntegral x] , x `mod` f == 0 , f' <- [f, x `div` f]]

isPrime :: Int -> Bool
isPrime x | length (factors x) == 2 = True
          | otherwise               = False

primesUpTo :: Int -> [Int]
primesUpTo x = [p | p <- [2..x] , isPrime p] 

problem = sum $ primesUpTo' 2000000

primesUpTo' :: Int -> [Int]
primesUpTo' x | x < 2     = []
              | x < 4     = [2,3]
              | otherwise = 2 : 3 : [p | i <- [1..x `div` 6] , let t = 6*i -1, let t' = 6*i +1 , p <- test t t' ]
                where
                  test :: Int -> Int -> [Int]
                  test x y | isPrime x && isPrime y = [x,y]
                           | isPrime x              = [x]
                           | isPrime y              = [y]
                           | otherwise              = []