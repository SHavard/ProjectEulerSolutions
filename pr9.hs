pythagTrip = [(sum [a,b,c], product [a,b,c]) | a <- [1..1000] , b <- [a+1..1000] , c <- [b+1..1000] , a^2+b^2==c^2 ]

-- pyTrip1000 = [t | t <- pythagTrip , t!!0 + t!!1 + t!!2 == 1000  ]
