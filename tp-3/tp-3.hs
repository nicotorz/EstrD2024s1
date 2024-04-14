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
ponerN n c celda = ponerN (n - 1) c (poner c celda) 

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
camino5 = Cofre [Cacharro,Tesoro] (Nada (Nada (Cofre [Tesoro,Tesoro,Tesoro,Cacharro,Cacharro] Fin)))
camino6 = Nada (Cofre [Cacharro,Tesoro] (Nada (Cofre [Tesoro,Tesoro,Tesoro,Cacharro,Cacharro] Fin)))

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
hayTesoroEn 0 c              = False
hayTesoroEn n (Fin)          = False
hayTesoroEn n (Nada c)       = hayTesoroEn (n-1) c
hayTesoroEn n (Cofre objs c) = if tieneTesoro objs && n==1
                                then True 
                                else False || hayTesoroEn (n-1) c

alMenosNTesoros :: Int -> Camino -> Bool
-- Indica si hay al menos n tesoros en el camino.
alMenosNTesoros 0 c              = True
alMenosNTesoros _ Fin            = False
alMenosNTesoros n (Nada c)       = alMenosNTesoros n c 
alMenosNTesoros n (Cofre objs c) = if cantidadTesoros objs >= n 
                                    then True 
                                    else alMenosNTesoros (n-(cantidadTesoros objs)) c

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

cantTesorosEntre :: Int -> Int -> Camino -> Int
{- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si el rango es 3 y 5, 
   indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están incluidos tanto 3 como 5 en el resultado.
-}
cantTesorosEntre _ _ Fin              = 0
cantTesorosEntre n1 n2 (Nada c)       = cantTesorosEntre (n1-1) (n2-1) c
cantTesorosEntre n1 n2 (Cofre objs c) = if (n1<=0 && n2/=0)
                                         then cantidadTesoros objs + cantTesorosEntre (n1-1) (n2-1) c
                                         else cantTesorosEntre (n1-1) (n2-1) c
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
leaves EmptyT = []  
leaves (NodeT a EmptyT EmptyT) = [a] 
leaves (NodeT a left right) = leaves left ++ leaves right
--7.
heightT :: Tree a -> Int
-- Dado un árbol devuelve su altura. Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad de niveles del árbol1 La altura para EmptyT es 0, y para una hoja es 1
heightT EmptyT = 0 
heightT (NodeT _ left right) = 1 + max (heightT left) (heightT right)
--8. 
mirrorT :: Tree a -> Tree a
-- Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del árbol.
mirrorT EmptyT = EmptyT 
mirrorT (NodeT x left right) = NodeT x (mirrorT right) (mirrorT left)
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
levelN n EmptyT          = []
levelN 0 (NodeT x _ _)   = x : []
levelN n (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2
--11.
listPerLevel :: Tree a -> [[a]]
-- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel EmptyT          = []
listPerLevel (NodeT a t1 t2) = [a] : juntarNiveles (listPerLevel t1) (listPerLevel t2)

juntarNiveles:: [[a]] -> [[a]] -> [[a]]
juntarNiveles [] yss            = yss
juntarNiveles xss []            = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss
--12.
ramaMasLarga :: Tree a -> [a]
-- Devuelve los elementos de la rama más larga del árbol. Si ambas ramas tienen el mismo tamaño elige la rama izquierda
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) =  if cantidadDeNodos t1 >= cantidadDeNodos t2  
                                    then x : leaves t1 
                                    else x : leaves t2

cantidadDeNodos :: Tree a -> Int
-- Devuelve la cantidad de nodos que posee el arbol.
cantidadDeNodos EmptyT = 0
cantidadDeNodos (NodeT x t1 t2) = 1 + cantidadDeNodos t1 + cantidadDeNodos t2

--13.
todosLosCaminos :: Tree a -> [[a]]
-- Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz hasta cualquiera de los nodos.
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) = [x] : (consACada x (todosLosCaminos t1)) ++ (consACada x (todosLosCaminos t2))

consACada :: a -> [[a]] -> [[a]]
consACada x [] = []
consACada x (xs:xss) = (x:xs) : consACada x xss

-- 2.2. Expresiones Aritmeticas

data ExpA = Valor Int| Sum ExpA ExpA| Prod ExpA ExpA | Neg ExpA
    deriving Show

suma0 = Sum (Valor 0) (Valor 2)
suma1 = Sum (Valor 20) (Valor 2)
prod0 = Prod (Valor 0) (Valor 10)
prod1 = Prod (Valor 1) (Valor 10)
prod2 = Prod (Valor 10) (Valor 10)
neg0  = Neg (Valor (-(-100)))
neg1  = Neg (Valor 15)
--1. 
eval :: ExpA -> Int
-- Dada una expresión aritmética devuelve el resultado evaluarla.
eval (Valor n)        = n
eval (Sum exp1 exp2)  = eval exp1 + eval exp2
eval (Prod exp1 exp2) = eval exp1 * eval exp2
eval (Neg exp)        = -(eval exp)
--2.
simplificar :: ExpA -> ExpA
{-
    Dada una expresión aritmética, la simplifica según los siguientes criterios s (descritos utilizando notación matemática convencional):
    a) 0 + x = x + 0 = x
    b) 0 * x = x * 0 = 0
    c) 1 * x = x * 1 = x
    d) - (- x) = x
-}
simplificar (Sum exp1 exp2)  = simplificarSum (simplificar exp1) (simplificar exp2)
simplificar (Prod exp1 exp2) = simplificarProd (simplificar exp1) (simplificar exp2)
simplificar (Neg exp)      = simplificarNeg (simplificar exp)
simplificar exp            = exp

simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum (Valor 0) exp2 = exp2
simplificarSum exp1 (Valor 0) = exp1
simplificarSum exp1 exp2 = Sum exp1 exp2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0) _ = Valor 0
simplificarProd _ (Valor 0) = Valor 0
simplificarProd (Valor 1) exp2 = exp2
simplificarProd exp1 (Valor 1) = exp1
simplificarProd exp1 exp2 = Prod exp1 exp2

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg exp) = exp
simplificarNeg exp = Neg exp
