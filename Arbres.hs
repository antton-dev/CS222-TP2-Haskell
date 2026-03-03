data Bintree a = 
    Leaf a |
    Node a (Bintree a) (Bintree a)
    deriving Show

-- On définit le type Bintree récursivement : Il peut s'agit d'une feuille (Leaf) qui représente la fin de l'arbre, ou bien d'un noeud interne (Node), qui contient deux fils, eux même du type Bintree (Bintree a).
-- a représente un type quelconque.

arbre_example :: Bintree Int
arbre_example  =  (Node (-8) (Node 18 (Leaf (-5)) (Leaf 42)) (Node 13 (Node 28 (Leaf 4) (Leaf 0)) (Leaf 73))) 


nombre_feuilles :: Bintree a -> Int
nombre_feuilles (Leaf _) = 1
nombre_feuilles (Node _ g d) = nombre_feuilles g + nombre_feuilles d 

hauteur :: Bintree a -> Int
hauteur (Leaf _) = 1
hauteur (Node _ g d) = 1 + max (hauteur g) (hauteur d)


abr_example :: Bintree Integer
abr_example = (Node 4 (Node (-5) (Leaf (-8)) (Leaf 0))   (Node 18 (Leaf 13) (Node 42 (Leaf 28) (Leaf 73))))


rechercher_abr :: Bintree Integer -> Integer -> Bool
rechercher_abr (Leaf v1) v2 | v1==v2  = True
                            | v1/=v2  = False
rechercher_abr (Node v1 g d) v2 | v1==v2   = True
                                | v2 < v1  = rechercher_abr g v2
                                | v2 > v1  = rechercher_abr d v2



aplatir_abr :: Bintree a -> [a]
aplatir_abr (Leaf v) = [v]
aplatir_abr (Node v g d) = (aplatir_abr g)++[v]++(aplatir_abr d)

-- Analyse de la rapidité / complexité : 
-- Cette recherche est plus rapide qu'une recherche naive dans une liste, consistant à parcourir les éléments les uns après les autres. 
-- Elle parait très similaire à une recherche dichotomique d'un tableau, car à chaque noeud, on "choisit" le sous-arbre gauche ou droite, de la même manière qu'on "choisit" le sous tableau inférieur ou supérieur dans une recherche dichotomique.
-- Cette recherche fonctionne bien si l'arbre est "équilibré" avec environ autant de fils à gauche qu'à droite.  