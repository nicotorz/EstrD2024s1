data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
    
bstTree :: Tree Int
bstTree = NodeT 5 
                (NodeT 3 
                       (NodeT 2 EmptyT EmptyT) 
                       (NodeT 4 EmptyT EmptyT)) 
                (NodeT 8 
                       (NodeT 6 EmptyT EmptyT) 
                       (NodeT 10 EmptyT EmptyT))

nonBSTTree :: Tree Int
nonBSTTree = NodeT 5 
                   (NodeT 3 
                          (NodeT 6 EmptyT EmptyT) 
                          (NodeT 4 EmptyT EmptyT)) 
                   (NodeT 8 
                          (NodeT 7 EmptyT EmptyT) 
                          (NodeT 10 EmptyT EmptyT))

belongsBST :: Ord a => a -> Tree a -> Bool
-- Propósito: dado un BST dice si el elemento pertenece o no al árbol.
-- Costo: O(log N)
belongsBST a EmptyT = False
belongsBST a (NodeT b ti td) = 
    if (a == b)       then True 
     else if (a > b)  then belongsBST a ti
                      else belongsBST a td
insertBST :: Ord a => a -> Tree a -> Tree a
-- Propósito: dado un BST inserta un elemento en el árbol.
-- Costo: O(log N)
insertBST a EmptyT          = NodeT a EmptyT EmptyT
insertBST a (NodeT b ti td) = 
    if (a == b)       then NodeT a ti td
     else if (a > b)  then NodeT b (insertBST a ti) td
                      else NodeT b ti (insertBST a td)
deleteBST :: Ord a => a -> Tree a -> Tree a
-- Propósito: dado un BST borra un elemento en el árbol.
-- Costo: O(log N)
deleteBST _ EmptyT          = EmptyT
deleteBST a (NodeT b ti td) = 
    if (a==b)         then rearmarBST ti td
     else if (a < b)  then NodeT b (deleteBST a ti) td
                      else NodeT b ti (deleteBST a td)

splitMinBST :: Ord a => Tree a -> (a, Tree a)
-- Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- Prec: El Arbol no esta Vacio
-- Costo: O(log N)
splitMinBST (NodeT a EmptyT td) = (a, td)
splitMinBST (NodeT a ti td)     = let (m, ti') = splitMinBST ti
                                  in (m, NodeT a ti' td)

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
-- Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
-- Prec: El Arbol no esta Vacio
-- Costo: O(log N)
splitMaxBST (NodeT a ti EmptyT) = (a, ti)
splitMaxBST (NodeT a ti td)     = let (m, td') = splitMaxBST td
                                  in (m, NodeT a ti td')

esBST :: Ord a => Tree a -> Bool
-- Propósito: indica si el árbol cumple con los invariantes de BST.
-- Costo: O(N2)
esBST EmptyT          = True
esBST (NodeT a ti td) = if esMayorT a ti && esMenorT a td
                         then esBST ti && esBST td
                         else False 
                     
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
-- Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado.
-- Costo: O(log N)
elMaximoMenorA _ EmptyT          = Nothing
elMaximoMenorA a (NodeT b ti td) = 
       if (a < b)           then buscarMaximoMenor a ti
                            else buscarMaximoMenor a td

{-
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
-- Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al elemento dado.
-- Costo: O(log N)

balanceado :: Tree a -> Bool
-- Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
-- Costo: O(N2)
-}

-- FUNCIONES AUXILIARES
root :: Tree a -> a 
root EmptyT       = error "El arbol esta vacio"
root (NodeT a _ _) = a
rearmarBST :: Ord a => Tree a -> Tree a -> Tree a 
-- Prec: Los dos arboles son BST
rearmarBST EmptyT td = td
rearmarBST ti td     = let (m, ti') = splitMaxBST ti
                        in NodeT m ti' td

esMenorT :: Ord a => a -> Tree a -> Bool
-- Proposito: indica si el valor dado es menor a la raiz del arbol dado.
esMenorT _ EmptyT        = True
esMenorT a (NodeT b _ _) = a < b

esMayorT :: Ord a => a -> Tree a -> Bool
-- Proposito: indica si el valor dado es mayor a la raiz del arbol dado.
esMayorT _ EmptyT        = True
esMayorT a (NodeT b _ _) = a > b

buscarMaximoMenor :: Ord a => a -> Tree a -> Maybe a 
buscarMaximoMenor _ EmptyT          = Nothing
buscarMaximoMenor a (NodeT b ti td) = if a < b 
                                          then buscarMaximoMenor a ti
                                          else Just (root ti)

buscarMaximoMayor :: Ord a => a -> Tree a -> Maybe a
buscarMaximoMayor _ EmptyT          = Nothing
buscarMaximoMayor a (NodeT b ti td) = if esMayorOIgualT a td
                                          then buscarMaximoMayor a td
                                          else Just (root ti)

esMenorOIgualT :: Ord a => a -> Tree a -> Bool
-- Proposito: indica si el valor dado es menor a la raiz del arbol dado.
esMenorOIgualT _ EmptyT        = True
esMenorOIgualT a (NodeT b _ _) = a <= b

esMayorOIgualT :: Ord a => a -> Tree a -> Bool
-- Proposito: indica si el valor dado es menor a la raiz del arbol dado.
esMayorOIgualT _ EmptyT        = True
esMayorOIgualT a (NodeT b _ _) = a >= b


