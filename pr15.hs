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

--Purely for readability (pretty bad practice- fight me)
numPermutations :: Integer -> Integer
numPermutations = factorial

-- total permutations of 20 accross 20 down, removing semantically equiv. permutations
problem = numPermutations (2*gridSize) `div` (numPermutations gridSize)^2
