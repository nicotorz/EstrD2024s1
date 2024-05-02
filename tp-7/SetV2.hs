module SetV2
    (Set, emptyS, addS, belongs, removeS, setToList)
   where
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

data Set a = Set (Tree a)
{- INV.REP.: En (S t), t cumple ser un BST -}

emptyS :: Set a -- O(1)
--Crea un conjunto vacío.
emptyS = S EmptyT

addS :: Ord a => a -> Set a -> Set a
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- Precon: t es un BST
addS a (S t) = S (insertarBST a t)

belongs :: Ord a => a -> Set a -> Bool
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
--Prec: t es BST
belongs a (S t) = buscarBST x t

removeS :: Ord a => a -> Set a -> Set a
--Borra un elemento del conjunto.
removeS a (S t) = S (borrarBST a t)

setToList :: Ord a => Set a -> [a] -- O (t 
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto
setToList (S t) = inorder t -- Consecuencia del Inv.Rep., la lista esta ordenada.

-- Funciones Auxiliares

inorder :: Tree a -> [a] -- O (n'2)
inorder EmptyT          = []
inorder (NodeT a ti td) = inorder ti ++ [a] ++ inorder td

buscarBST :: Ord a => a -> Tree a -> Bool -- O (n) en peor caso.
                                          -- O (log n) en promedio, si es un arbol balanceado, solo recorre una rama.
-- Prec: el arbol es BST 
buscarBST a EmptyT          = False
buscarBST a (NodeT b ti td) = 
    if (a == b)             then True
        else if (a < b)     then buscarBST a ti
                            else buscarBST a td

insertarBST :: Ord a => a -> Tree a -> Tree a
-- Prec: el arbol es BST 
insertarBST a EmptyT          = NodeT a EmptyT EmptyT
insertarBST a (NodeT b ti td) =
    if (a == b)             then NodeT a ti td
        else if (a < b)     then NodeT y (insertarBST a ti) td
                            else NodeT y ti (insertarBST a td)

borrarBST :: Ord a => a -> Tree a -> Tree a
-- Prec: el arbol es BST 
borrarBST a EmptyT          = 
borrarBST a (NodeT b ti td) = 
    if (a == b)             then ti 
        else if (a < b)     then NodeT y (borrarBST a ti) td
                            else NodeT y ti (borrarBST a td)

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
-- Prec: los arboles son BST.
rearmarBST EmptyT td = td
rearmarBST ti td     = let (m, ti') = splitMaxBST ti
                        in NodeT m ti' td

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
-- Prec: el arbol es BST, y NO esta vacio
splitMaxBST (NodeT x ti EmptyT) = (x, ti)
splitMaxBST (NodeT x ti td)     = let (m, td') = splitMaxBST td
                                    in (m, NodeT x ti td')