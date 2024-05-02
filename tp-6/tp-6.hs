import PriorityQueue
import Map
-- PRIORITY QUEUE 

heapSort :: Ord a => [a] -> [a]
-- Dada una lista la ordena de menor a mayor utilizando una Priority Queue como estructura auxiliar.
heapSort [] = []
heapSort xs = let rest = deleteMinList xs
              in 
            findMinPQ (listToPQ xs) : heapSort rest

listToPQ :: Ord a => [a] -> PriorityQueue a
-- Dada una lista de a la transforma en una Priorrity Queue de a 
listToPQ []     = emptyPQ
listToPQ (a:as) = insertPQ a (listToPQ as) 

toList :: Ord a => PriorityQueue a -> [a]
toList pq = if isEmptyPQ pq 
             then []
             else findMinPQ pq : toList (deleteMinPQ pq)

deleteMinList :: Ord a => [a] -> [a]
-- Proposito: Devuelve una lista sin el elemento minimo en ella.
deleteMinList as = borrar (minimun as) as

minimun :: Ord a => [a] -> a
minimun [] = error "La lista esta vacia, no hay minimo"
minimun [x] = x
minimun (x:xs) = min x (minimun xs)

borrar :: Ord a => a -> [a] -> [a]
-- Proposito: Devuelve una lista sin el elemento pasado por parametro, si ese elemento no existe, devuelve la lista como esta.
borrar a []        = []
borrar a1 (a2:as2) = if a1 == a2 
                        then as2
                        else a2 : borrar a1 as2

-- MAPS

valuesM :: Eq k => Map k v -> [Maybe v]
-- Propósito: obtiene los valores asociados a cada clave del map.
valuesM emptyM = []
valuesM map     = let listaDeClavesEnMap = keys map
                 in valoresAsociadosPara listaDeClavesEnMap map

valoresAsociadosPara :: Eq k => [k] -> Map k v -> [Maybe v]
valoresAsociadosPara [] _       = []
valoresAsociadosPara (k:ks) map = lookupM k map : valoresAsociadosPara ks map

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas [] _       = True
todasAsociadas (k:ks) map = if perteneceK k map
                              then True && todasAsociadas ks map
                              else False

perteneceK :: Eq k => k -> Map k v -> Bool
-- Proposito: Indica si la clave dada se encuentra en el map
perteneceK k map = case lookupM k map of
                            Nothing -> False
                            Just _  -> True

listToMap :: Eq k => [(k, v)] -> Map k v
--Propósito: convierte una lista de pares clave valor en un map.
listToMap []            = emptyM
listToMap ((k,v) : kvs) = assocM k v (listToMap kvs)

mapToList :: Eq k => Map k v -> [(k, v)]
--Propósito: convierte un map en una lista de pares clave valor.
mapToList map    = let keysOnMap = keys map
                   in mapToListAux (keysOnMap) map

mapToListAux :: Eq k => [k] -> Map k v -> [(k, v)]
--Propósito: convierte un map en una lista de pares clave valor, dadas las claves del map.
mapToListAux [] _ = []
mapToListAux (k:ks) map = case lookupM k map of
                             Nothing -> mapToListAux ks map
                             Just v  -> (k, v) : mapToListAux ks map

agruparEq :: Eq k => [(k, v)] -> Map k [v]
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq [] = emptyM
agruparEq ((k, v):kvs) = assocM k (v : valoresRestantes k kvs) (agruparEq kvs)

valoresRestantes :: Eq k => k -> [(k, v)] -> [v]
-- Propósito: Devuelve los valores asociados a una clave excluyendo el primer par de la lista.
valoresRestantes _ [] = []
valoresRestantes k ((k', v'):kvs) = if k == k' 
                                     then v' : valoresRestantes k kvs
                                     else valoresRestantes k kvs

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
incrementar [] map = map
incrementar (k:ks) map =
    case lookupM k map of
        Nothing -> incrementar ks map
        Just n  -> incrementar ks (assocM k (n + 1) (deleteM k map))

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si una clave del primero existe en el segundo, es reemplazada por la del primero


