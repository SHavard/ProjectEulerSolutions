factors :: Int -> [Int]
factors x = sort $ 1 : x : [p' | p <- [2.. ceiling $ sqrt $ fromIntegral x] , mod x p == 0 , p' <- [p , div x p]]

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs , y <= x] ++ [x] ++ sort [y | y <- xs , y > x]

isPrime :: Int -> Bool
isPrime x | (length $ factors x) == 2 = True
          | otherwise                 = False

primeFactors :: Int -> [Int]
primeFactors x = filter (isPrime) (factors x)

problem = last $ primeFactors 600851475143
