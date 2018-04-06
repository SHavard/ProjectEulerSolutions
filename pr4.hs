import Data.List

isPalindrome :: Int -> Bool
isPalindrome x | length (show x) == 1 = True
               | length (show x) == 0 = error "no input"
               | otherwise            = help $ show x
                 where
                  help s = and [b | i <- [0..length s -1] , let b = if s!!i==(s' s)!!i then True else False]
                  s' s = reverse s

palindromes :: [Int]
palindromes = sort $ nub [p | x <- [999, 998..100] , y <- [x, x-1..100] , let p = x*y , isPalindrome p]

problem = last $ sort palindromes
