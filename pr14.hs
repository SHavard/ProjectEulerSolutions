collatzSeq :: Int -> [Int]
collatzSeq x | x == 1 = [1]
             | even x = x : collatzSeq (x `div` 2)
             | odd x  = x : collatzSeq (3*x + 1)
             
chainLength :: Int -> (Int, Int)
chainLength x = (length $ collatzSeq x, x)

under1M = [p | i <- [1000000, 999999..1] , let p = chainLength i]

problem = maximum under1M