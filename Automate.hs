data EtatMachine = Vide | Percolation | CafePret Int deriving (Show, Eq)
type InfoMachine = (Int, EtatMachine)

lancerMachine :: InfoMachine -> InfoMachine
lancerMachine (c, state) | state == Vide  =  (c, Percolation)
                         | otherwise      =  (c, state)

attendre :: InfoMachine -> InfoMachine
attendre (c, Percolation) = (c, CafePret 4)
attendre (c, state) = (c, state)

servirCafe :: InfoMachine -> InfoMachine
servirCafe (c, CafePret n) | n>0  = (c+1, CafePret (n-1)) 
                           | otherwise = (c, Vide)
servirCafe (c, state) = (c, state)


executerActions :: InfoMachine -> [Char] -> InfoMachine
executerActions info chaine = 
