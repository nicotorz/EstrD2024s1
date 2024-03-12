-- Numeros Enteros
-- 1.a
sucesor :: Int -> Int 
sucesor n = n + 1
--1.b
sumar :: Int -> Int -> Int
sumar n m = n + m
--1.c
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (n `div` m, n `mod` m)
    -- PREC: m no puede ser igual a 0
--1.d
maximo :: Int -> Int -> Int
maximo n m =
    if n < m 
        then m
        else n 

maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = maximo n m

{- 
    2) De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
        · divisionYResto (maxDelPar (100,10)) (sucesor (sumar 5 4)) - Da como resultado una tupla donde el divisor es 10 y su resto es 0. 
        · maxDelPar (divisionYResto (sucesor 99) (sumar 9 1))
        · maxDelPar (divisionYResto (sumar (sucesor 544) (sucesor 454)) (sucesor 99))
        · maxDelPar (divisionYResto (sucesor (sumar 101 100)) (sumar (sucesor 9) (sucesor 9)) )
-}

