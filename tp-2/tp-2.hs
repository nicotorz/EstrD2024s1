--1. RECURSIÓN SOBRE LISTAS
--1.1
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
--1.2
longitud ::[a] -> Int
longitud [] = 0
longitud (_:as) = 1 + longitud as
--1.3
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = sucesor x : sucesores xs
sucesor :: Int -> Int 
sucesor n = n + 1
--1.4
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (b:bs) = b && conjuncion bs
--1.5
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b:bs) = b || disyuncion bs
--1.6
aplanar :: [[a]] -> [a]
aplanar [] = [] 
aplanar (a:as) = a ++ aplanar as 
--1.7
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = (a==x) || pertenece a xs
--1.8
apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0 
apariciones a (x:xs) = if (a==x)
                        then 1 + apariciones a xs
                        else apariciones a xs
--1.9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA n (x:xs) = if n>x
                       then x : losMenoresA n xs
                       else losMenoresA n xs
--Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n
--1.10
{- PARA TERMINAR
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (a:as) = if longitud a > n 
-}
--1.11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] a = [a]
agregarAlFinal (x:xs) a = x : agregarAlFinal xs a 
--1.12
agregar :: [a] -> [a] -> [a]
agregar [] y = y
agregar a [] = a
agregar (a:as) y = a : agregar as y 
--1.13
reversa :: [a] -> [a]
reversa [] = []
reversa (a:as) = agregar (reversa as) [a]
--1.14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] xs = xs
zipMaximos xs [] = xs
zipMaximos (x:xs) (y:ys) = maximo x y : zipMaximos xs ys
maximo :: Int -> Int -> Int
maximo x y = if x > y 
             then x
             else y
--1.15
elMinimo :: Ord a => [a] -> a
elMinimo [] = error "La lista está vacía, no hay mínimo"
elMinimo [x] = x
elMinimo (x:xs) = min x (elMinimo xs)

--2. FACTORIAL
--2.1
factorial :: Int -> Int
factorial 0 = 1 
factorial n = n * factorial (n-1)
--Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
--2.2
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = 
        if n < 1 
            then []
            else n : cuentaRegresiva (n-1)
--Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
--2.3
repetir :: Int -> a -> [a]
    --PRECOND: El numero no puede ser negativo - (Int >= 0)
repetir 0 _ = []
repetir n a = a : repetir (n-1) a
--Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
--2.4
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs 
--Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs. Si la lista es vacía, devuelve una lista vacía.
--2.5
sinLosPrimeros :: Int -> [a] -> [a]
    --PRECOND: El numero debe ser mayor o igual a la longitud de la lista.
sinLosPrimeros 0 a = a
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs
--Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista recibida. Si n es cero, devuelve la lista completa.

--REGISTROS
--1. Persona
data Persona = P String Int
    deriving Show
edad :: Persona -> Int
edad (P _ e) = e
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = 
    if esMayorQueLaOtra p1 p2
        then p1
        else p2
--1.a
mayoresA :: Int -> [Persona] -> [Persona]
--Dados una edad y una lista de personas devuelve a las personas mayores a esa edad
mayoresA _ [] = []
mayoresA n (p:ps) = if edad p > n
                        then p : mayoresA n ps
                        else mayoresA n ps
--1.b
promedioEdad :: [Persona] -> Int
promedioEdad [] = error "La lista es vacia"
promedioEdad ps = div (sumatoriaDeEdades ps) (longitud ps)
--Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
sumatoriaDeEdades :: [Persona] -> Int
sumatoriaDeEdades [] = 0
sumatoriaDeEdades (p:ps) = edad p + sumatoriaDeEdades ps
--1.c
elMasViejo :: [Persona] -> Persona
--Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la lista al menos posee una persona.
elMasViejo [] = error "La lista es vacia"
elMasViejo [p] = p
elMasViejo (p:ps) = laQueEsMayor p (elMasViejo ps)
nicolas = P "Nicolas" 24
santino = P "Santino" 9
benja = P "Benja" 5 
eze = P "Ezequiel" 29

--2. Entrenador y Pokemon
data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]
