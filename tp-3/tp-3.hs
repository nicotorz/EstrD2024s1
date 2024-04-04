-- Tipos recursivos simples
-- 1.1 Celdas con bolitas

data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

nroBolitas :: Color -> Celda -> Int
-- Dados un color y una celda, indica la cantidad de bolitas de ese color.
nroBolitas _ CeldaVacia = 0
nroBolitas c1 (Bolita c2 celda) = unoSi (esMismoColor c1 c2) + nroBolitas c1 celda

esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _ _       = False

unoSi :: Bool -> Int
unoSi True = 1
unoSi _    = 0

poner :: Color -> Celda -> Celda 
-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner c celda = Bolita c celda

sacar :: Color -> Celda -> Celda
-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de Gobstones, esta función es total.
sacar c1 CeldaVacia     = CeldaVacia
sacar c1 (Bolita c2 celda) = if esMismoColor c1 c2    
                              then celda
                              else (Bolita c2 (sacar c1 celda))

ponerN :: Int -> Color -> Celda -> Celda
-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN 0 _ celda = celda
ponerN n c1 (Bolita c2 celda) = ponerN (n-1) c1 (Bolita c2 (poner c1 celda))
                               

celda0 = Bolita Rojo (Bolita Azul (Bolita Rojo CeldaVacia))
celda1 = Bolita Rojo CeldaVacia

--1.2 Camino hacia el tesoro

data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

camino0 = Fin
camino1 = Nada Fin
camino2 = Cofre [Tesoro] Fin
camino3 = Nada (Nada (Nada (Nada(Cofre [Tesoro] Fin))))
camino4 = Cofre [Cacharro] (Nada (Nada (Cofre [Tesoro,Tesoro,Tesoro,Cacharro,Cacharro] Fin)))

hayTesoro :: Camino -> Bool
-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro Fin            = False
hayTesoro (Cofre objs c) = if tieneTesoro objs 
                            then True
                            else hayTesoro c
hayTesoro (Nada c)       = hayTesoro c

tieneTesoro :: [Objeto] -> Bool
-- Indica si hay al menos un tesoro.
tieneTesoro [] = False
tieneTesoro (obj:objs) = esTesoro obj || tieneTesoro objs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

pasosHastaTesoro :: Camino -> Int
-- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro. Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
-- Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro Fin            = error "No hay tesoros en este camino"
pasosHastaTesoro (Cofre objs c) = if tieneTesoro objs
                                    then 0 
                                    else 1 + pasosHastaTesoro c
pasosHastaTesoro (Nada c)       = 1 + pasosHastaTesoro c

hayTesoroEn :: Int -> Camino -> Bool
-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn n c = pasosHastaTesoro c == n

alMenosNTesoros :: Int -> Camino -> Bool
-- Indica si hay al menos n tesoros en el camino.
alMenosNTesoros n c = cantTesorosEn c >= n

cantTesorosEn :: Camino -> Int
-- Retorna la cantidad de tesoros que hay en el camino.
cantTesorosEn Fin            = 0 
cantTesorosEn (Cofre objs c) = cantidadTesoros objs + cantTesorosEn c
cantTesorosEn (Nada c)       = cantTesorosEn c

cantidadTesoros :: [Objeto] -> Int
-- Retorna la cantidad de Tesoros que hay en una lista de objetos.
cantidadTesoros [] = 0
cantidadTesoros (obj:objs) = if esTesoro obj
                                then 1 + cantidadTesoros objs
                                else cantidadTesoros objs

--cantTesorosEntre :: Int -> Int -> Camino -> Int
{- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si el rango es 3 y 5, 
   indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están incluidos tanto 3 como 5 en el resultado.
-}

-- 2. Tipos arboreos
-- 2.1 Arboles Binarios

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

a2 :: Tree Int
a2 = NodeT 1 
        (NodeT 2
            (NodeT 1 EmptyT EmptyT)
            EmptyT) 
        (NodeT 1 
            EmptyT EmptyT)
a3 = NodeT 1
        (NodeT 2
            (NodeT 4 EmptyT EmptyT)
            (NodeT 5 EmptyT EmptyT))
        (NodeT 3
            EmptyT
            (NodeT 6 EmptyT EmptyT))
--1.
sumarT :: Tree Int -> Int
-- Dado un árbol binario de enteros devuelve la suma entre sus elementos
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2
--2.
sizeT :: Tree a -> Int
-- Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol.
sizeT EmptyT          = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2
--3. 
mapDobleT :: Tree Int -> Tree Int
-- Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2) = NodeT (2*n) (mapDobleT t1) (mapDobleT t2)
--4.
perteneceT :: Eq a => a -> Tree a -> Bool
-- Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
perteneceT _ EmptyT           = False
perteneceT a (NodeT a2 t1 t2) = pertenece a a2 || perteneceT a t1 || perteneceT a t2

pertenece :: Eq a => a -> a -> Bool
pertenece a1 a2 = if a1 == a2
                    then True
                    else False
--5.
aparicionesT :: Eq a => a -> Tree a -> Int
-- Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
aparicionesT _ EmptyT           = 0 
aparicionesT a (NodeT a2 t1 t2) = unoSi (pertenece a a2) + aparicionesT a t1 + aparicionesT a t2  
--6.
leaves :: Tree a -> [a]
-- Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves EmptyT          = []
leaves (NodeT a t1 t2) = a : leaves t1 ++ leaves t2
--7.
heightT :: Tree a -> Int
-- Dado un árbol devuelve su altura. Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad de niveles del árbol1 La altura para EmptyT es 0, y para una hoja es 1
heightT EmptyT = 0 
heightT (NodeT a t1 t2) = 1 + heightT t1 + heightT t2
--8. 
mirrorT :: Tree a -> Tree a
-- Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del árbol.
mirrorT EmptyT = EmptyT
mirrorT (NodeT a t1 t2) = (NodeT a t2 t1)
--9.
toList :: Tree a -> [a]
-- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order. Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz y luego los elementos del hijo derecho.
toList EmptyT = []
toList (NodeT a t1 t2) = leaves t1 ++ [a] ++ leaves t2
--10
levelN :: Int -> Tree a -> [a]
{-Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un nodo es la distancia que hay de la raíz hasta él. 
La distancia de la raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1. Nota: El primer nivel de un árbol (su raíz) es 0.
-}
levelN n EmptyT = []
levelN n (NodeT a t1 t2) = if n == 0
                            then [a]
                            else levelN (n - 1) t1 ++ levelN (n - 1) t2
--11.
listPerLevel :: Tree a -> [[a]]
-- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel EmptyT = []
listPerLevel t = listPerLevelOn t ((levelsOnT t) - 1)

listPerLevelOn :: Tree a -> Int -> [[a]]
-- Dado un arbol y un numero devuelve una lista de listas de los nodos n en la que cada elemento representa un nivel de dicho arbol.
listPerLevelOn EmptyT n = []
listPerLevelOn (NodeT a _ _) 0 = [[a]]
listPerLevelOn t n = if n < 0 
                        then []
                        else levelN n t : listPerLevelOn t (n-1)

levelsOnT :: Tree a -> Int
-- Dado un arbol retorna la cantidad de niveles que posee. 
levelsOnT EmptyT = 0 
levelsOnT (NodeT _ t1 t2) = 1 + max (levelsOnT t1) (levelsOnT t2) 


tomarHasta :: Int -> [a] -> [a]
tomarHasta 0 xs     = []
tomarHasta _ []     = []
tomarHasta n (a:as) = a : tomarHasta (n-1) as

tomarDesde :: Int -> [a] -> [a]
tomarDesde 0 as     = as
tomarDesde _ []     = []
tomarDesde n (a:as) = tomarDesde (n-1) as

tomarEntre :: Int -> Int -> [a] -> [a]
tomarEntre x y xs = tomarHasta (y-x+1) (tomarDesde x xs)