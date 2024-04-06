data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving (Show, Eq)
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving (Show, Eq)
pizza0 = Prepizza
pizza1 = Capa Salsa Prepizza
pizza2 = Capa Salsa 
            (Capa Queso Prepizza)
pizza3 = Capa Salsa 
            (Capa Queso
            (Capa Jamon Prepizza))
pizza4 = Capa Salsa 
            (Capa Queso 
            (Capa Jamon 
            (Capa (Aceitunas 8) Prepizza)))


cantidadDeCapas :: Pizza -> Int
-- Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

armarPizza :: [Ingrediente] -> Pizza
-- Dada una lista de ingredientes construye una pizza
armarPizza []     = Prepizza
armarPizza (i:is) = (Capa i (armarPizza is))

sacarJamon :: Pizza -> Pizza
-- Le saca los ingredientes que sean jamón a la pizza
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = if esJamon i 
                         then sacarJamon p 
                         else (Capa i (sacarJamon p))

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

tieneSoloSalsaYQueso :: Pizza -> Bool
{-
Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
-}
tieneSoloSalsaYQueso Prepizza   = True
tieneSoloSalsaYQueso (Capa i p) = if esSalsaOQueso i 
                                    then True && tieneSoloSalsaYQueso p
                                    else False

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _     = False

duplicarAceitunas :: Pizza -> Pizza
-- Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = if esAceituna i
                                then Capa (aceitunasDuplicadas i) (duplicarAceitunas p)
                                else Capa i (duplicarAceitunas p)

aceitunasDuplicadas :: Ingrediente -> Ingrediente
aceitunasDuplicadas (Aceitunas x) = (Aceitunas (x*2))
aceitunasDuplicadas ing           = ing

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas n) = True
esAceituna _             = False

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
-- Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = agregar p ++ cantCapasPorPizza ps

agregar :: Pizza -> [(Int, Pizza)]
agregar p = [((cantidadDeCapas p), p)]