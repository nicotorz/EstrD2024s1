module Multiset
    (MultiSet, emptyM, assocM, lookupM, deleteM, keys)
    where
import Map
data MultiSet a = MS (Map a Int)
    deriving Show


emptyMS :: MultiSet a
-- Propósito: denota un multiconjunto vacío.
emptyMS = MS (emptyM)
addMS :: Ord a => a -> MultiSet a -> MultiSet a
-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
addMS a (MS map) = MS (assocM a (ocurrencesMS a (MS map) + 1) map)
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
ocurrencesMS a (MS map) = case lookupM a map of 
                          Just n  -> n
                          Nothing -> 0
                          
multiSetToList :: Eq a => MultiSet a -> [(a, Int)]
-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su cantidad de ocurrencias.
multiSetToList (MS map) = mapToList map

-- FUNCIONES AUXILIARES 
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
