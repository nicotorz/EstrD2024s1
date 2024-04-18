module Stack
    (Stack, emptyS', isEmptyS, push, top, pop, lenS)
   where

data Stack a = S [a] n
{- INV.REP.: en S as n
    * si ts es vacia, n es 0.
    * si ts NO es vacia, n es la cantidad de elementos en as
-}

emptyS' :: Stack a
-- Crea una pila vacía.
emptyS = S [] 0
isEmptyS :: Stack a -> Bool
-- Dada una pila indica si está vacía.
isEmptyS (S _ 0) = True
isEmptyS _       = False
push :: a -> Stack a -> Stack a
-- Dados un elemento y una pila, agrega el elemento a la pila.
push a (S (xs) n) = S (a:xs) n+1
top :: Stack a -> a
-- Dada un pila devuelve el elemento del tope de la pila.
top (S _ 0) = error "No hay elementos en la pila"
top (S xs n) = ultimoElementoEn xs 
pop :: Stack a -> Stack a
-- Dada una pila devuelve la pila sin el primer elemento.
pop (S [] n)   = (S [] n)
pop (S (x:xs)) = (S xs n-1) 
lenS :: Stack a -> Int
-- Dada la cantidad de elementos en la pila.
-- Costo: constante.
lenS (S xs n) = n

-- Funciones auxiliares
ultimoElementoEn :: [a] -> a
-- Dada una lista devuelve el ultimo elemento de dicha lista
-- Prec: la lista no debe ser vacia 
ultimoElementoEn []      = error "la lista es vacia"
ultimoElementoEn [x]     = x
ultimoElementoEn (x:xs)  = ultimoElementoEn xs