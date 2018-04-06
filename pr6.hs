sumOfSq = sum [x*x | x <- [1..100]]

sqOfSum = sum [1..100] ^ 2

problem = sqOfSum - sumOfSq
