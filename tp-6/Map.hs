module Map
    (Map, emptyM, assocM, lookupM, deleteM, keys)
    where
data Map k v = Map [(k, v)]
    deriving Show

emptyM :: Map k v
-- Propósito: devuelve un map vacío
emptyM = Map [] 

assocM :: Eq k => k -> v -> Map k v -> Map k v
-- Propósito: agrega una asociación clave-valor al map.
assocM k v (Map kvs) = Map (asociar k v kvs)

lookupM :: Eq k => k -> Map k v -> Maybe v
-- Propósito: encuentra un valor dado una clave.
lookupM k (Map kvs) = buscar k kvs

deleteM :: Eq k => k -> Map k v -> Map k v
-- Propósito: borra una asociación dada una clave.
deleteM k (Map kvs) = Map (borrar k kvs)

keys :: Eq k => Map k v -> [k]
-- Propósito: devuelve las claves del map.
keys (Map kvs) = claves kvs


-- FUNCIONES AUXILIARES 
buscar :: Eq k => k -> [(k,v)] -> Maybe v
-- Busca la clave dada en la lista de pares (clave,valor) y devuelve su valor asociado
buscar k []              = Nothing 
buscar k ((k',v') : kvs) = if k == k'  
                            then Just v'
                            else buscar k kvs

{-clavesSinRepetidos :: Eq k => [(k,v)] -> [k]
-- Dada una lista de pares (clave,valor) retorna las claves sin repetidos.
clavesSinRepetidos []            = []
clavesSinRepetidos ((k,v) : kvs) = if pertenece k kvs
                                        then clavesSinRepetidos kvs
                                        else k : clavesSinRepetidos kvs
-}
claves :: Eq k => [(k,v)] -> [k]
-- Dada una lista de pares (clave, valor) retorna las claves sin repetidos
claves []            = [] 
claves ((k,v) : kvs) = k : claves kvs
pertenece :: Eq k => k -> [(k,v)] -> Bool
-- Indica si existe la clave dada en la lista de pares (clave,valor).
pertenece k []              = False
pertenece k ((k',v') : kvs) = if k == k'
                                then True
                                else pertenece k kvs

borrar :: Eq k => k -> [(k,v)] -> [(k,v)]
-- Proposito: Elimina todas las claves dadas en la lista de pares (clave,valor).
borrar k [] = []
borrar k ((k',v') : kvs) = if k == k'
                            then borrar k kvs
                            else (k',v') : borrar k kvs

asociar ::Eq k => k -> v -> [(k,v)] -> [(k,v)]
asociar k v []            = [ (k,v) ]
asociar k v ((k',v'):kvs) = if k == k' 
                                then (k',v) : kvs
                                else (k',v') : asociar k v kvs 