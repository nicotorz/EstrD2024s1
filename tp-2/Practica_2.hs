--1. RECURSIÓN SOBRE LISTAS
--1.1
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
--1.2
longitud ::[a] -> Int
longitud [] = 0
longitud (a:as) = 1 + longitud as
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