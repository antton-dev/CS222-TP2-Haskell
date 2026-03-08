data Bintree a = 
    Leaf a |
    Node a (Bintree a) (Bintree a)
    deriving Show

abr_example :: Bintree Integer
abr_example = (Node 4 (Node (-5) (Leaf (-8)) (Leaf 0))   (Node 18 (Leaf 13) (Node 42 (Leaf 28) (Leaf 73))))


parcours_infixe :: Bintree a -> [a]
parcours_infixe (Leaf v) = [v]
parcours_infixe (Node v g d) = (parcours_infixe g) ++ [v] ++ (parcours_infixe d)


foldr_arbre :: (a -> b -> b) -> b -> Bintree a -> b
foldr_arbre f acc (Leaf x) = f x acc
foldr_arbre f acc (Node v gauche droite) = 
    let acc_droite = foldr_arbre f acc droite
        acc_noeud  = f v acc_droite
    in foldr_arbre f acc_noeud gauche


to_list :: Foldable t => t a -> [a]
to_list = foldr (:) []


instance Foldable Bintree where
    foldr = foldr_arbre

-- la fonction sum prend n'importe quelle structure Foldable t contenant des nombres (Num a) et renvoie un nombre de ce même type
-- son type est donc : (Foldable t, Num a) => t a -> a