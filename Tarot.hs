data Couleur = Piques | Coeur | Carreau | Trefle deriving (Show, Eq)
data Carte = Standard Int Couleur | Atout Int | Excuse deriving Show

comparer :: Carte -> Carte -> Bool

comparer (Standard _ _) (Atout _) = False
comparer (Atout v1) (Atout v2) = v1 > v2
comparer (Atout _) _ = True
comparer _ (Atout _) = False
comparer Excuse Excuse = False
comparer _ Excuse = True 
comparer Excuse _ = False
comparer (Standard v1 c1) (Standard v2 c2) | c1==c2    =  v1 > v2
                                           | otherwise =  False



meme_couleur :: Couleur -> Carte -> Bool
meme_couleur c1 ((Standard _ c2 )) | c1==c2    =  True               
                                   | otherwise =  False
meme_couleur _ _ = False

atout_plus_grand :: Int -> Carte -> Bool
atout_plus_grand v1 (Atout v2) | v2 > v1   = True
                               | otherwise = False

atout :: Carte -> Bool
atout (Atout _) = True
atout _ = False

(|||) :: [a] -> [a] -> [a]
(|||) l1 l2 | length l1 /= 0   = l1
            | otherwise  = l2

cartes_possibles_hors_excuse :: Carte -> [Carte] -> [Carte]
cartes_possibles_hors_excuse (Standard _ c) deck = filter (meme_couleur c)  deck ||| filter atout deck ||| deck 
cartes_possibles_hors_excuse (Atout v ) deck = filter (atout_plus_grand v) deck ||| filter atout deck ||| deck


cartes_possibles :: Carte -> [Carte] -> [Carte]
cartes_possibles played deck = sans_excuse ++ excuses 
    where 
        excuses = filter (est_excuse) deck 
        deck_restant = filter (not_excuse) deck

        sans_excuse = cartes_possibles_hors_excuse played deck_restant 


        est_excuse Excuse = True
        est_excuse _      = False

        not_excuse Excuse = False
        not_excuse _      = True