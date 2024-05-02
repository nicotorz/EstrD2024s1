# TP-5
## 1. Calculo de costos 

### Costo constante
###### head' :: [a] -> a <br />
###### head' (x:xs) = x 
### Costo Constante
###### sumar :: Int -> Int <br />
###### sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
### Costo lineal
###### factorial :: Int -> Int
###### factorial 0 = 1
###### factorial n = n * factorial (n-1)
### Costo lineal
###### longitud :: [a] -> Int
###### longitud [] = 0
###### longitud (x:xs) = 1 + longitud xs
### Costo cuadratico
###### factoriales :: [Int] -> [Int]
###### factoriales [] = []
###### factoriales (x:xs) = factorial x : factoriales xs
### Costo lineal
###### pertenece :: Eq a => a -> [a] -> Bool
###### pertenece n [] = False
###### pertenece n (x:xs) = n == x || pertenece n xs
### Costo cuadratico
###### sinRepetidos :: Eq a => [a] -> [a]
###### sinRepetidos [] = []
###### sinRepetidos (x:xs) = if pertenece x xs
######                         then sinRepetidos xs
######                         else x : sinRepetidos xs
### Costo lineal                        
###### -- equivalente a (++)
###### append :: [a] -> [a] -> [a]
###### append [] ys = ys
###### append (x:xs) ys = x : append xs ys
### Costo cuadratico
###### concatenar :: [String] -> String
###### concatenar [] = []
###### concatenar (x:xs) = x ++ concatenar xs
### Costo lineal
###### takeN :: Int -> [a] -> [a]
###### takeN 0 xs = []
###### takeN n [] = []
###### takeN n (x:xs) = x : takeN (n-1) xs
### Costo lineal
###### dropN :: Int -> [a] -> [a]
###### dropN 0 xs = xs
###### dropN n [] = []
###### dropN n (x:xs) = dropN (n-1) xs
### Costo cuadratico
###### partir :: Int -> [a] -> ([a], [a])
###### partir n xs = (takeN n xs, dropN n xs)
### Costo cuadratico
###### minimo :: Ord a => [a] -> a
###### minimo [x] = x
###### minimo (x:xs) = min x (minimo xs)
### Costo lineal
###### sacar :: Eq a => a -> [a] -> [a]
###### sacar n [] = []
###### sacar n (x:xs) = if n == x
######                     then xs
######                     else x : sacar n xs
### Costo cubico
###### ordenar :: Ord a => [a] -> [a]
###### ordenar [] = []
###### orderar xs =
######         let m = minimo xs
######         in m : ordenar (sacar m xs)