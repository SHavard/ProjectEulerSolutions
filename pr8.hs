import Data.Char

-- digitsRev :: Int -> [Int]
-- digitsRev 0 = []
-- digitsRev x = [x `mod` 10] ++ digitsRev (x `div` 10)

digits :: String -> [Int]
digits s = [x | c <- s , c /= '\n' , let x = ord c - 48]

products :: [Int] -> [Int]
products xs | length xs < 13  = error "input too small"
            | length xs == 13 = [product xs]
            | otherwise      = [product ys | i <- [0..length xs -13] , let ys = take13 i xs ]
  where
    take13 i xs | i == 0 = take 13 xs
                | otherwise = take13 (i-1) (tail xs)

maxProduct x = maximum $ products $ digits x
