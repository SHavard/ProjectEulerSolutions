gridSize = 20

-- 2 possible moves
-- right || down
-- => no backtracking/ visiting same point more than once

-- Start always in top left of square
-- End always in bottom right of square
-- => every path must step right gridSize times
-- AND every path must step down gridSize times

-- => number of possible paths from start to end
--     = number of permutations of gridSize rights and gridSize downs ignoring duplicates

factorial :: Integer -> Integer
factorial x = product [1..x]

numPermutations :: Integer -> Integer
numPermutations = factorial

problem = numPermutations (2*gridSize) `div` (numPermutations gridSize)^2
