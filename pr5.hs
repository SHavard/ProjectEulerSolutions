byAll x = and [b | y <- [1..20] , let b = if mod x y == 0 then True else False]

problem = head [x | x <- [20,40..] , byAll x]
