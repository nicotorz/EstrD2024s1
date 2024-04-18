import Set
import Queue
import Stack

-- SET
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen [] set     = [] 
losQuePertenecen (x:xs) set = if belongs x set 
                                then x : losQuePertenecen xs set
                                else losQuePertenecen xs set

sinRepetidos :: Eq a => [a] -> [a]
-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos xs = setToList(listToSet xs)

listToSet :: Eq a => [a] -> Set a
listToSet []     = emptyS 
listToSet (x:xs) = addS x (listToSet xs)

{-unirTodos :: Eq a => Tree (Set a) -> Set a
-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
unirTodos EmptyT          = emptyS
unirTodos (NodeT s ti td) = unionS s (unionS (unirTodos ti) (unirTodos td))-}

-- ===========================================================================
-- ===========================================================================
-- ===========================================================================
-- ===========================================================================
-- QUEUE
lengthQ :: Queue a -> Int
-- Cuenta la cantidad de elementos de la cola.
lengthQ q = if isEmptyQ q
             then 0
             else 1 + lengthQ (dequeue q)

queueToList :: Queue a -> [a]
-- Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
-- Nota: chequear que los elementos queden en el orden correcto
queueToList q = if isEmptyQ q
                then []
                else firstQ q : (queueToList (dequeue q))

unionQ :: Queue a -> Queue a -> Queue a
-- Inserta todos los elementos de la segunda cola en la primera.
unionQ q1 q2 = if isEmptyQ q2
               then q1
               else let q1ConElPrimeroDeQ2 = (enqueue (firstQ q2) q1 ) 
                        q2YaSinElPrimero = dequeue q2
                    in
                    unionQ q1ConElPrimeroDeQ2 q2YaSinElPrimero

-- ===========================================================================
-- ===========================================================================
-- ===========================================================================
-- ===========================================================================
-- STACK
apilar :: [a] -> Stack a
-- Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar []      = emptyS'
aplilar (x:xs) = push x (apilar xs) 

desapilar :: Stack a -> [a]
-- Dada una pila devuelve una lista sin alterar el orden de los elementos
desapilar sk = if emptyS' sk
                then []
                else top sk : desapilar (pop sk)

insertarEnPos :: Int -> a -> Stack a -> Stack a
-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos 0 a sk = push a sk
insertarEnPos n a sk = let elementoTope   = top st
                           pilaSinPrimero = pop st 
                       in
                      push elementoTope (insertarEnPos (n-1) e pilaSinPrimero)
