module PriorityQueue 
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
   where
data PriorityQueue a = PQ [a]

emptyPQ :: PriorityQueue a
-- Propósito: devuelve una priority queue vacía.
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool
-- Propósito: indica si la priority queue está vacía.
isEmptyPQ (PQ xs) = null xs

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
-- Propósito: inserta un elemento en la priority queue.
insertPQ a (PQ as) = PQ (a:as)

findMinPQ :: Ord a => PriorityQueue a -> a
-- Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
-- Precondición: parcial en caso de priority queue vacía.
findMinPQ (PQ as) = minimun as

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
-- Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
-- Precondición: parcial en caso de priority queue vacía
deleteMinPQ (PQ as) = (PQ (deleteMinList as))

-- FUNCIONES AUXILIARES
deleteMinList :: Ord a => [a] -> [a]
-- Proposito: Devuelve una lista sin el elemento minimo en ella.
deleteMinList as = borrar (minimun as) as

borrar :: Ord a => a -> [a] -> [a]
-- Proposito: Devuelve una lista sin el elemento pasado por parametro, si ese elemento no existe, devuelve la lista como esta.
borrar a []        = []
borrar a1 (a2:as2) = if a1 == a2 
                        then as2
                        else a2 : borrar a1 as2

minimun :: Ord a => [a] -> a
minimun [] = error "La lista está vacía, no hay mínimo"
minimun [x] = x
minimun (x:xs) = min x (minimun xs)