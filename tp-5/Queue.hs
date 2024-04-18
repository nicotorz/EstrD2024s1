module Queue 
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
   where

data Queue a = Queue [a]

emptyQ :: Queue a
--Crea una cola vacía.
emptyQ                = Queue []
isEmptyQ :: Queue a -> Bool
--Dada una cola indica si la cola está vacía.
isEmptyQ (Queue [])   = True
isEmptyQ _            = False
enqueue :: a -> Queue a -> Queue a
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue a (Queue xs)  = Queue (xs ++ [a])
firstQ :: Queue a -> a
--Dada una cola devuelve el primer elemento de la cola.
firstQ (Queue xs)     = head xs
dequeue :: Queue a -> Queue a
--Dada una cola la devuelve sin su primer elemento.
dequeue (Queue [])    = (Queue [])
dequeue (Queue xs)    = Queue (sinElPrimero xs)

-- Funciones Auxiliares 
sinElPrimero :: [a] -> [a]
sinElPrimero []     = []
sinElPrimero (x:xs) = xs


