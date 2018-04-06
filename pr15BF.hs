data Move   = R | D                               deriving (Show)
data Path   = Steps Move Path | Move Move | Empty deriving (Show)
newtype Pos = Pos (Int,Int)                       deriving (Show, Eq)

gridSize = 20

startPos = Pos (0,0)
goalPos  = Pos (gridSize, gridSize)

makeMove :: Pos -> Move -> Maybe Pos
makeMove (Pos (x,y)) R | x < gridSize = Just (Pos (x+1,y))
                       | otherwise    = Nothing
makeMove (Pos (x,y)) D | y < gridSize = Just (Pos (x, y+1))
                       | otherwise    = Nothing

possibleMoves :: Pos -> [Move]
possibleMoves p = [m | m <- [R, D] , makeMove p m /= Nothing]

explorePaths :: Pos -> [Path]
explorePaths p = help p Empty
  where
    help :: Pos -> Path -> [Path]
    help l p | l == goalPos = [p]
             | otherwise    = [p' | m <- possibleMoves l , p' <- help (unwrap $ makeMove l m) (Steps m p)]

-- USED TO WORK OUT HOW TO "DFS" EXPLORE FROM START TO GOAL
--ADAPTED INTO explorePaths
-- findPath :: Pos -> Path -> Path
-- findPath l p | l == goalPos            = p
--              | makeMove l R /= Nothing = findPath (unwrap $ makeMove l R) (Steps R p)
--              | makeMove l D /= Nothing = findPath (unwrap $ makeMove l D) (Steps D p)

unwrap :: Maybe Pos -> Pos
unwrap (Just x) = x
unwrap Nothing  = Pos (-1,-1)

-- USED FOR TESTING DISCOVERED PATHS
-- pathToSeq :: Path -> [Move]
-- pathToSeq (Empty)     = []
-- pathToSeq (Move m)    = [m]
-- pathToSeq (Steps m p) = pathToSeq p ++ [m]
--
-- pathsToSeqs :: [Path] -> [[Move]]
-- pathsToSeqs [] = []
-- pathsToSeqs (x:xs) = pathToSeq x : pathsToSeqs xs

problem = length $ explorePaths startPos
