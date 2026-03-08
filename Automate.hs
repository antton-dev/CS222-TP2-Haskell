data EtatMachine = Vide | Percolation | CafePret Int deriving (Show, Eq)
type InfoMachine = (Int, EtatMachine)

lancerMachine :: InfoMachine -> InfoMachine
lancerMachine (c, state) | state == Vide  =  (c, Percolation)
                         | otherwise      =  (c, state)

attendre :: InfoMachine -> InfoMachine
attendre (c, Percolation) = (c, CafePret 4)
attendre (c, state) = (c, state)

servirCafe :: InfoMachine -> InfoMachine
servirCafe (c, CafePret 1) = (c + 1, Vide)
servirCafe (c, CafePret n) | n > 1 = (c + 1, CafePret (n - 1))
servirCafe info = info


executerActions :: InfoMachine -> [Char] -> InfoMachine
executerActions info [] = info
executerActions info ('A':xs) = executerActions (attendre info) xs
executerActions info ('L':xs) = executerActions (lancerMachine info) xs
executerActions info ('S':xs) = executerActions (servirCafe info) xs
executerActions info (_:xs)   = executerActions info xs 

-- En executant la séquence, on a servi 9 cafés et il reste 3 tasses pretes.
