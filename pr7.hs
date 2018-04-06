import Data.List

factors :: Int -> [Int]
factors x = nub $ 1 : x : [p' | p <- [2.. ceiling $ sqrt $ fromIntegral x] , mod x p == 0 , p' <- [p , div x p]]

isPrime :: Int -> Bool
isPrime x | length (factors x) == 2 = True
          | otherwise               = False

primes = [x | x <- 2:[3,5..] , isPrime x]

problem = primes!!10000
