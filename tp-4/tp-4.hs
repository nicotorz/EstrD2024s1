-- 1. PIZZAS
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

--2. MAPA DE TESOROS
data Dir = Izq | Der
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre = Cofre [Objeto]
    deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show

mapa0 = Fin (Cofre [Chatarra])
mapa1 = Bifurcacion (Cofre [Chatarra, Chatarra])
            (Bifurcacion (Cofre [Chatarra])
                (Fin (Cofre []))
                (Fin (Cofre []))
            )
            (Fin (Cofre [Tesoro]))
mapa2 = Bifurcacion (Cofre [Tesoro]) 
        (Bifurcacion (Cofre [Tesoro,Chatarra,Chatarra]) 
            (Bifurcacion (Cofre [Tesoro, Chatarra, Tesoro]) 
                (Fin (Cofre [Tesoro])) 
                (Fin (Cofre [Chatarra]))
            ) 
            (Fin (Cofre [Tesoro]))
        ) 
        (Fin (Cofre [Chatarra]))
--1.
hayTesoro :: Mapa -> Bool
-- Indica si hay un tesoro en alguna parte del mapa
hayTesoro (Fin c)               = tieneTesoro c
hayTesoro (Bifurcacion c m1 m2) = tieneTesoro c || hayTesoro m1 || hayTesoro m2

tieneTesoro :: Cofre -> Bool
tieneTesoro (Cofre objs) = existeTesoroEn objs

existeTesoroEn :: [Objeto] -> Bool
existeTesoroEn [] = False
existeTesoroEn (obj:objs) = if esTesoro obj
                             then True
                             else existeTesoroEn objs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False
--2.
hayTesoroEn :: [Dir] -> Mapa -> Bool
{- 
    Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una lista vacía de direcciones.
-}
hayTesoroEn _  (Fin c)                   = tieneTesoro c
hayTesoroEn [] (Bifurcacion c _ _)       = tieneTesoro c
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = if (esIzq d) 
                                            then hayTesoroEn ds m1
                                            else hayTesoroEn ds m2

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _   = False
--3.
caminoAlTesoro :: Mapa -> [Dir]
-- Indica el camino al tesoro. Precondición: existe un tesoro y es único
caminoAlTesoro (Fin c)               = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoro m1 
                                        then Izq : caminoAlTesoro m1
                                        else Der : caminoAlTesoro m2
--4. 
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga mapa    = caminoAlFin (ramaMasLargaEn mapa)

ramaMasLargaEn :: Mapa -> Mapa
-- Devuelve la rama mas larga de un mapa .
ramaMasLargaEn (Fin c)               = (Fin c)
ramaMasLargaEn (Bifurcacion _ m1 m2) = if (cantidadDeNodos m1) > (cantidadDeNodos m2)
                                            then m1
                                            else m2
cantidadDeNodos :: Mapa -> Int
-- Devuelve la cantidad de nodos que posee el arbol.
cantidadDeNodos (Fin _) = 0
cantidadDeNodos (Bifurcacion _ m1 m2) = 1 + cantidadDeNodos m1 + cantidadDeNodos m2

caminoAlFin :: Mapa -> [Dir]
caminoAlFin (Fin _)               = []
caminoAlFin (Bifurcacion _ m1 m2) = if (cantidadDeNodos m1) >= (cantidadDeNodos m2)
                                        then Izq : caminoAlFin m1
                                        else Der : caminoAlFin m2
--5.
tesorosPorNivel :: Mapa -> [[Objeto]]
-- Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel (Fin c)               = [tesorosEn c]
tesorosPorNivel (Bifurcacion c m1 m2) = tesorosEn c : tesorosPorNivel m1 ++ tesorosPorNivel m2

tesorosEn :: Cofre -> [Objeto]
tesorosEn (Cofre objs) = tesorosEnList objs

tesorosEnList :: [Objeto] -> [Objeto]
tesorosEnList [] = []
tesorosEnList (obj:objs) = if esTesoro obj
                            then obj : tesorosEnList objs
                            else tesorosEnList objs
--6.
todosLosCaminos :: Mapa -> [[Dir]]
-- Devuelve todos lo caminos en el mapa.
todosLosCaminos (Fin _)                           = [[]]
todosLosCaminos (Bifurcacion _ izquierda derecha) =
    consACada Izq (todosLosCaminos izquierda) ++ consACada Der (todosLosCaminos derecha)

consACada :: a -> [[a]] -> [[a]]
consACada x [] = []
consACada x (xs:xss) = (x:xs) : consACada x xss

--3. NAVE ESPACIAL
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show
data Sector = S SectorId [Componente] [Tripulante]
    deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
data Nave = N (Tree Sector)
    deriving Show
sector1 :: Sector
sector1 = S "Puente" [Motor 3, LanzaTorpedos] ["Capitan"]
sector2 :: Sector
sector2 = S "Sala de almacenamiento" [Almacen [Comida, Oxigeno]] ["Logistico"]
sector3 :: Sector
sector3 = S "Segunda sala de almacenamiento " [Almacen [Comida, Oxigeno, Combustible, Combustible]] ["Gestor"]
sector4 :: Sector
sector4 = S "Sala Trasera" [Motor 90, LanzaTorpedos] ["Gestor"]

nave0 = N   (NodeT sector1
              (NodeT sector2
                      (NodeT sector3
                              (NodeT sector4 EmptyT EmptyT)
                              EmptyT)
                      EmptyT)
              EmptyT
        )
--1.
sectores :: Nave -> [SectorId]
-- Propósito: Devuelve todos los sectores de la nave.
sectores (N t) = sectoresT t 

sectoresT :: Tree Sector -> [SectorId]
sectoresT EmptyT          = []
sectoresT (NodeT s t1 t2) = idSector s : sectoresT t1 ++ sectoresT t2

idSector :: Sector -> SectorId 
idSector (S sId _ _) = sId
--2. 
poderDePropulsion :: Nave -> Int 
-- Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota: el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion (N t) = propulsionDeMotoresT t

propulsionDeMotoresT :: Tree Sector -> Int
propulsionDeMotoresT EmptyT = 0
propulsionDeMotoresT (NodeT s t1 t2) = propulsionDeMotor s + propulsionDeMotoresT t1 + propulsionDeMotoresT t2

propulsionDeMotor :: Sector -> Int 
propulsionDeMotor (S _ cs _) = propulsionDeMotorEn cs 

propulsionDeMotorEn :: [Componente] -> Int 
propulsionDeMotorEn []     = 0
propulsionDeMotorEn (c:cs) = cantidadDePropulsionEn c 

cantidadDePropulsionEn :: Componente -> Int 
cantidadDePropulsionEn (Motor n) = n 
cantidadDePropulsionEn _         = 0
--3.
barriles :: Nave -> [Barril]
-- Propósito: Devuelve todos los barriles de la nave.
barriles (N t) = barrilesT t

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT          = []
barrilesT (NodeT s t1 t2) = barrilesEnS s ++ barrilesT t1 ++ barrilesT t2

barrilesEnS :: Sector -> [Barril]
barrilesEnS (S _ cs _) = barrilesEnC cs 

barrilesEnC :: [Componente] -> [Barril]
barrilesEnC [] = []
barrilesEnC (c:cs) = listaDeBarrilesEn c 

listaDeBarrilesEn :: Componente -> [Barril]
listaDeBarrilesEn (Almacen brs) = brs
listaDeBarrilesEn _              = []
--4.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector cs sectorid (N tree)  = (N (treeConComponentesAgregados cs sectorid tree))

treeConComponentesAgregados :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
treeConComponentesAgregados cs sectorid EmptyT = EmptyT
treeConComponentesAgregados cs sectorid (NodeT sector s1 s2) = if esMismoSector sectorid sector 
                                                                then (NodeT (agregarComponentesAsector cs sector) s1 s2)
                                                                else (NodeT sector (treeConComponentesAgregados cs sectorid s1) (treeConComponentesAgregados cs sectorid s2))

esMismoSector :: SectorId  -> Sector -> Bool
esMismoSector sid (S sid2 _ _) = sid == sid2

agregarComponentesAsector :: [Componente] -> Sector -> Sector
agregarComponentesAsector [] s = s 
agregarComponentesAsector cs (S sectorid css tripulantes) = S sectorid (cs ++ css) tripulantes
--5.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA t sids (N tree) = (N (treeConTripulanteAgregado t sids tree))

treeConTripulanteAgregado :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
treeConTripulanteAgregado t sids EmptyT = EmptyT
treeConTripulanteAgregado t sids (NodeT sector s1 s2) = if pertenece (idSector sector) sids 
                                                        then (NodeT (sectorConTripulanteAgregado t sector) (treeConTripulanteAgregado t sids s1) (treeConTripulanteAgregado t sids s2))
                                                        else (NodeT sector (treeConTripulanteAgregado t sids s1) (treeConTripulanteAgregado t sids s2))

sectorConTripulanteAgregado :: Tripulante -> Sector -> Sector
sectorConTripulanteAgregado t (S sid cs ts) = (S sid cs (t:ts))
--6.
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
-- Propósito: Devuelve los sectores en donde aparece un tripulante dado
sectoresAsignados t (N tree) = sectoresAsignadosT t tree

sectoresAsignadosT :: Tripulante -> Tree Sector -> [SectorId]
-- Proposito: Devuelve los sectores donde aparece un tripulante dado en el arbol de sectores.
sectoresAsignadosT t EmptyT         = [] 
sectoresAsignadosT t (NodeT s t1 t2) = if existeEn t s 
                                        then idSector s : sectoresAsignadosT t t1 ++ sectoresAsignadosT t t2
                                        else sectoresAsignadosT t t1 ++ sectoresAsignadosT t t2
existeEn :: Tripulante -> Sector -> Bool
-- Proposito: Indica si existe el tripulante en el sector dado.
existeEn t (S _ _ []) = False
existeEn t (S _ _ ts) = hayAlgunEn t ts

hayAlgunEn :: Tripulante -> [Tripulante] -> Bool
hayAlgunEn t []       = False
hayAlgunEn t (t1:t1s) = if t == t1 
                            then True
                            else False || hayAlgunEn t t1s
--7.
tripulantes :: Nave -> [Tripulante]
-- Propósito: Devuelve la lista de tripulantes, sin elementos repetidos
tripulantes (N t) = tripulantesT t

tripulantesT :: Tree Sector -> [Tripulante]
-- Propósito: Devuelve la lista de tripulantes, sin elementos repetidos
tripulantesT EmptyT          = []
tripulantesT (NodeT s t1 t2) = agregarTripulanteSinRepetidos s (tripulantesT t1 ++ tripulantesT t2)

agregarTripulanteSinRepetidos :: Sector -> [Tripulante] -> [Tripulante]
agregarTripulanteSinRepetidos (S _ _ t1s) t2s = sinRepetidos t1s t2s 

sinRepetidos :: Eq a => [a] -> [a] -> [a]
sinRepetidos [] as       = as
sinRepetidos as []       = as
sinRepetidos (a:as1) as2 = if pertenece a as2
                            then sinRepetidos as1 as2
                            else a : sinRepetidos as1 as2

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = (a==x) || pertenece a xs

--4. MANADA DE LOBOS 
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
    deriving Show
data Manada = M Lobo
    deriving Show
--1.
construirManada :: Manada
construirManada = M cazadorInicial
  where
    -- Cazador con sus presas
    cazadorInicial :: Lobo
    cazadorInicial = Cazador "Cazador Alfa" ["Ciervo", "Liebre"] explorador1 explorador2 cria1
    -- Exploradores con territorios
    explorador1 = Explorador "Explorador 1" ["Bosque", "Cueva"] cria2 cria3
    explorador2 = Explorador "Explorador 2" ["Rio", "Cueva"] cria4 cria5
    -- Crías
    cria1 = Cria "Cria 1"
    cria2 = Cria "Cria 2"
    cria3 = Cria "Cria 3"
    cria4 = Cria "Cria 4"
    cria5 = Cria "Cria 5"
cazadorInicial :: Lobo
cazadorInicial = Cazador "Cazador Alfa" ["Ciervo", "Liebre"] explorador1 explorador2 cria1
    where
    -- Exploradores con territorios
    explorador1 = Explorador "Explorador 1" ["Bosque", "Cueva"] cria2 cria3
    explorador2 = Explorador "Explorador 2" ["Rio", "Cueva"] cria4 cria5
    -- Crías
    cria1 = Cria "Cria 1"
    cria2 = Cria "Cria 2"
    cria3 = Cria "Cria 3"
    cria4 = Cria "Cria 4"
    cria5 = Cria "Cria 5"
explorador1 :: Lobo
explorador1 = Explorador "Explorador 1" ["Bosque", "Cueva"] cria2 cria3
    where
    -- Crías
    cria1 = Cria "Cria 1"
    cria2 = Cria "Cria 2"
    cria3 = Cria "Cria 3"
    cria4 = Cria "Cria 4"
    cria5 = Cria "Cria 5"
explorador2 :: Lobo
explorador2 = Explorador "Explorador 2" ["Rio", "Cueva"] cria4 cria5
    where
    -- Crías
    cria1 = Cria "Cria 1"
    cria2 = Cria "Cria 2"
    cria3 = Cria "Cria 3"
    cria4 = Cria "Cria 4"
    cria5 = Cria "Cria 5"

--2.
buenaCaza :: Manada -> Bool
-- Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías
buenaCaza m = cantidadDeAlimento m > cantidadDeCrias m

cantidadDeAlimento :: Manada -> Int
cantidadDeAlimento (M l) = cantidadDeAlimentoL l

cantidadDeAlimentoL :: Lobo -> Int
cantidadDeAlimentoL (Cazador _ presas l1 l2 l3) = alimentoEn presas + cantidadDeAlimentoL l1 + cantidadDeAlimentoL l2 + cantidadDeAlimentoL l3 
cantidadDeAlimentoL (Explorador _ _ l1 l2)      = cantidadDeAlimentoL l1 + cantidadDeAlimentoL l2
cantidadDeAlimentoL (Cria _)                    = 0

alimentoEn :: [Presa] -> Int
alimentoEn ps = lenght ps

cantidadDeCrias :: Manada -> Int 
cantidadDeCrias (M l) = cantidadDeCriasL l

cantidadDeCriasL :: Lobo -> Int 
cantidadDeCriasL (Cazador _ presas l1 l2 l3) = cantidadDeCriasL l1 + cantidadDeCriasL l2 + cantidadDeCriasL l3 
cantidadDeCriasL (Explorador _ _ l1 l2)      = cantidadDeCriasL l1 + cantidadDeCriasL l2
cantidadDeCriasL (Cria _)                    = 1

esCria :: Lobo -> Bool
esCria (Cria _) = True
esCria _        = False

unoSi :: Bool -> Int
unoSi True = 1 
unoSi _    = 0

lenght :: [a] -> Int
lenght []     = 0 
lenght (a:as) = 1 + lenght as
--3
elAlfa :: Manada -> (Nombre, Int)
{-
    Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
    con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
    cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
    cero presas
-}
elAlfa (M l) = elAlfaL l

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cazador n prs l1 l2 l3)  = elegirEntre (n, alimentoEn prs)
                                                (elegirEntre (elAlfaL l1)
                                                     (elegirEntre (elAlfaL l2)
                                                                  (elAlfaL l3)))
elAlfaL (Explorador n _ l1 l2)    = elegirEntre (elAlfaL l1)
                                                (elegirEntre (elAlfaL l2)
                                                            (n, 0))
elAlfaL (Cria n)                  = (n, 0) 

elegirEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elegirEntre (nom1, c1) (nom2, c2) = if c1 >= c2
                                        then (nom1, c1)
                                        else (nom2, c2)

--4. 
losQueExploraron :: Territorio -> Manada -> [Nombre]
-- Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que pasaron por dicho territorio.
losQueExploraron t (M l) = losLQueExploraron t l

losLQueExploraron :: Territorio -> Lobo -> [Nombre]
-- Propósito: dado un territorio y una lobo, devuelve los nombres de los exploradores que pasaron por dicho territorio.
losLQueExploraron _ (Cria _)                = []
losLQueExploraron t (Cazador _ _ l1 l2 l3)  = losLQueExploraron t l1 ++ losLQueExploraron t l2 ++ losLQueExploraron t l3
losLQueExploraron t (Explorador n ts l1 l2) = if pertenece t ts 
                                               then n : losLQueExploraron t l1 ++ losLQueExploraron t l2
                                               else losLQueExploraron t l1 ++ losLQueExploraron t l2
--5.
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
{- Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
   dicho territorio. Los territorios no deben repetirse.
-}
exploradoresPorTerritorio m = exploradoresPorTerritorioDeLista (territoriosM m) m

territoriosM :: Manada -> [Territorio]
territoriosM (M l) = sinTerritoriosRepetidos(todosLosTerritoriosDe l)

exploradoresPorTerritorioDeLista :: [Territorio] -> Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorioDeLista [] m     = []
exploradoresPorTerritorioDeLista (t:ts) m = (t, losQueExploraron t m) : exploradoresPorTerritorioDeLista ts m

todosLosTerritoriosDe :: Lobo -> [Territorio]
todosLosTerritoriosDe (Cazador n ps l1 l2 l3) = todosLosTerritoriosDe l1 ++ 
                                                todosLosTerritoriosDe l2 ++ 
                                                todosLosTerritoriosDe l3

todosLosTerritoriosDe (Explorador n ts l1 l2) = ts ++ todosLosTerritoriosDe l1 
                                                ++ todosLosTerritoriosDe l2

todosLosTerritoriosDe (Cria n)                = []


sinTerritoriosRepetidos :: [Territorio] -> [Territorio]
sinTerritoriosRepetidos []     = []
sinTerritoriosRepetidos (t:ts) = if estaTerritorioEn t ts
                                 then sinTerritoriosRepetidos ts
                                 else t : sinTerritoriosRepetidos ts

estaTerritorioEn :: Territorio -> [Territorio] -> Bool
estaTerritorioEn t []       = False
estaTerritorioEn t (tr:trs) = esMismoTerritorio t tr || estaTerritorioEn t trs

esMismoTerritorio :: Territorio -> Territorio -> Bool
esMismoTerritorio t1 t2 = t1 == t2

-- 6
-- Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los cazadores que tienen como subordinado al cazador dado (directa o indirectamente).
-- Precondición: hay un cazador con dicho nombre y es único.
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M l) = losSuperioresAlCazadorDe n l

losSuperioresAlCazadorDe :: Nombre -> Lobo -> [Nombre]
losSuperioresAlCazadorDe nombre (Cria n)                = []
losSuperioresAlCazadorDe nombre (Cazador n ps l1 l2 l3) = if nombre == n
                                                          then []
                                                          else  n :
                                                          (losSuperioresAlCazadorDe nombre l1) ++
                                                          (losSuperioresAlCazadorDe nombre l2) ++
                                                          (losSuperioresAlCazadorDe nombre l3)
losSuperioresAlCazadorDe nombre (Explorador n ts l1 l2) = (losSuperioresAlCazadorDe nombre l1) ++ (losSuperioresAlCazadorDe nombre l2)