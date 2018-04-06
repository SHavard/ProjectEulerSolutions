multiples :: Int -> Int -> [Int]
multiples x lim = [v | v <- [1..lim] , mod v x == 0]

rmdupesSorted :: Ord a => [a] -> [a]
rmdupesSorted xs = rmdupe [] (sort xs)
  where
    sort [] = []
    sort (x:xs) = sort [x' | x' <- xs , x' <= x] ++ [x] ++ sort [x'' | x'' <- xs , x'' > x]
    rmdupe seen [] = seen
    rmdupe seen (x:xs) | elem x seen = rmdupe seen xs
                       | otherwise   = rmdupe (seen ++ [x]) xs

problem :: Int
problem = sum $ rmdupesSorted ((multiples 3 999) ++ (multiples 5 999))
