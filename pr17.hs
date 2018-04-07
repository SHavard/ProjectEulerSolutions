-- only works in range 1..1000 inclusive
-- al that was needed for this problem so unnecessary to fix
toWords :: Integer -> String
toWords x | x < 10    = digitsToWords 0 [x]
          | x < 100   = digitsToWords 1 (digitsRev x)
          | x < 1000  = digitsToWords 2 (digitsRev x)
          | x < 10000 = digitsToWords 3 (digitsRev x)
  where
    digitsRev :: Integer -> [Integer]
    digitsRev 0 = []
    digitsRev x = x `mod` 10 : digitsRev (x `div` 10)

    --Only need to go to x10^3 for this problem
    digitsToWords :: Int -> [Integer] -> String
    digitsToWords _ [] = ""
    digitsToWords p (x:xs) | p == 0                                       = digit x
                           | p == 1 && head xs == 1 && x /= 0             = teen x
                           | p == 1                                       = ten (xs!!0) ++ digit x
                           | p == 2 && x == 0 && xs!!0 == 0               = hundred (xs!!1)
                           | p == 2                                       = hundred (xs!!1) ++ "And" ++ digitsToWords 1 [x,xs!!0]
                           | p == 3 && x == 0 && xs!!0 == 0 && xs!!1 == 0 = thousand (xs!!2)
                           | p == 3                                       = thousand (xs!!2) ++ "And" ++ digitsToWords 2 [x,xs!!0,xs!!1]

    digit :: Integer -> String
    digit 0 = ""
    digit 1 = "One"
    digit 2 = "Two"
    digit 3 = "Three"
    digit 4 = "Four"
    digit 5 = "Five"
    digit 6 = "Six"
    digit 7 = "Seven"
    digit 8 = "Eight"
    digit 9 = "Nine"
    digit x = error "Out of range 0-9"
    teen :: Integer -> String
    teen 1 = "Eleven"
    teen 2 = "Twelve"
    teen 3 = "Thirteen"
    teen 4 = "Fourteen"
    teen 5 = "Fifteen"
    teen 6 = "Sixteen"
    teen 7 = "Seventeen"
    teen 8 = "Eighteen"
    teen 9 = "Nineteen"
    ten :: Integer -> String
    ten 0 = ""
    ten 1 = "Ten"
    ten 2 = "Twenty"
    ten 3 = "Thirty"
    ten 4 = "Forty"
    ten 5 = "Fifty"
    ten 6 = "Sixty"
    ten 7 = "Seventy"
    ten 8 = "Eighty"
    ten 9 = "Ninety"
    ten x = error "Out of range 0-9"
    hundred :: Integer -> String
    hundred 0 = ""
    hundred x = digit x ++ "Hundred"
    thousand :: Integer -> String
    thousand 0 = ""
    thousand x = digit x ++ "Thousand"

wordLengths :: [String] -> [Int]
wordLengths xs = [length s | s <- xs]

problem x = sum $ wordLengths [w | i <- [1..x] , let w = toWords i]
