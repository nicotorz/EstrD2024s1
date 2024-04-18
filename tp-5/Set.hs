module Set 
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
   where

data Set a = Set [a]

emptyS :: Set a
--Crea un conjunto vacío.
emptyS             = Set []
addS :: Eq a => a -> Set a -> Set a
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS a (Set as)    = if elem a as
                        then (Set as) 
                        else (Set (a:as)) 
belongs :: Eq a => a -> Set a -> Bool
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs a (Set as) = elem a as
sizeS :: Eq a => Set a -> Int
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (Set as)     = lenght as
removeS :: Eq a => a -> Set a -> Set a
--Borra un elemento del conjunto.
removeS a (Set as) = (Set (remove a as))
unionS :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS (Set xs) s = agregarS xs s
setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto
setToList (Set as) = as

-- Funciones Auxiliares
remove :: Eq a => a -> [a] -> [a]
remove a []        = []
remove a1 (a2:a2s) = if a1 == a2
                        then a2s
                        else a2 : remove a1 a2s 

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) =
    x : removeDuplicates (filter (/= x) xs) 

lenght :: [a] -> Int
lenght []     = 0 
lenght (a:as) = 1 + lenght as

agregarS :: Eq a => [a] -> Set a -> Set a
agregarS []     s = s
agregarS (x:xs) s =  addS x (agregarS xs s) 