halve :: [Int] -> ([Int], [Int])
halve [] = ([],[])
halve [x] = ([x], [])
halve (x:y:tail) = (x:xs, y:ys) where (xs, ys) = halve tail


combine :: [Int] -> [Int] -> [Int]
combine [] [] = []
combine [] l = l
combine l [] = l
combine (x:xs) (y:ys) | x < y     =  x : (combine xs (y:ys))
                      | otherwise =  y : (combine ys (x:xs))

tri_fusion :: [Int] -> [Int]
tri_fusion [] = []
tri_fusion [a] = [a]
tri_fusion l = combine (tri_fusion inf) (tri_fusion sup)  
               where (inf, sup) = halve l
