fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

fibonaccis :: Int -> [Int]
fibonaccis x = [fibonacci v | v <- [0..x]]

fibsUpTo :: Int -> [Int]
fibsUpTo m = fibs 0 (m-1)
  where
    fibs :: Int -> Int -> [Int]
    fibs x m | last (fibonaccis x) > m = fibonaccis (x-1)
             | last (fibonaccis x) < m = fibs (x+1) m
             | otherwise             = fibonaccis x

problem = sum $ filter (even) (fibsUpTo 4000000)
