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
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []     = [] 
lasDeLongitudMayorA n (a:as) = if longitud a > n 
                                then a : lasDeLongitudMayorA n as 
                                else lasDeLongitudMayorA n as 
--1.11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] a     = [a]
agregarAlFinal (x:xs) a = x : agregarAlFinal xs a 
--1.12
agregar :: [a] -> [a] -> [a]
agregar [] y = y
agregar a [] = a
agregar (a:as) y = a : agregar as y 
--1.13
reversa :: [a] -> [a]
reversa []     = []
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
--Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva 0 = []
cuentaRegresiva n = if n < 1 
                        then []
                        else n : cuentaRegresiva (n-1)
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
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
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
--Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
promedioEdad [] = error "La lista es vacia"
promedioEdad ps = div (sumatoriaDeEdades ps) (longitud ps)

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

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador n poke) = longitud poke

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tp (ConsEntrenador _ poke) = cantPokemonDeTipoEn tp poke

cantPokemonDeTipoEn :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDeTipoEn _ [] = 0
cantPokemonDeTipoEn tp (p:ps) = if mismoTipoDePokemon tp (tipoDePokemon p)
                                    then 1 + cantPokemonDeTipoEn tp ps
                                    else cantPokemonDeTipoEn tp ps

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ poke) = tienePokemonesDeTodosLosTipos poke

tienePokemonesDeTodosLosTipos :: [Pokemon] -> Bool
-- recibe una lista de pokemones e indica si esta misma posee pokemones de los tipos Agua, Fuego y Planta.
tienePokemonesDeTodosLosTipos [] = False
tienePokemonesDeTodosLosTipos poke =   (cantPokemonDeTipoEn Fuego poke > 0)  
                                    && (cantPokemonDeTipoEn Agua poke > 0)
                                    && (cantPokemonDeTipoEn Planta poke > 0)
{- cantPokemonDeTipoEn Fuego poke > 0, cantPokemonDeTipoEn Agua poke > 0, cantPokemonDeTipoEn Planta poke > 0 
        Podrian ser subtareas que indiquen si el entrenador o la lista de pokemones tiene al menos un pokemon de determiando tipo
-}
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ tp e1 e2 = cantidadDePokemonesGanadoresDeTipo tp (pokemonesDeEntrenador e1) (pokemonesDeEntrenador e2)
{-
La implementacion de cuantosDeTipo_De_LeGananATodosLosDe_ esta centrada en peleas 1 a 1 entre los pokemones de los entrenadores,
es decir, el primer pokemon que esta en la lista del entrenador1 peleara con el primer pokemon de la lista del entrenador2
siguiendo esas reglas de pelea en esta implementacion se quiere que al pasarle un tipo de pokemon te indique con un numero
la cantidad de pokemones de ese tipo que le ganarian a su contricante, esto quiere decir, por ejemplo : 
que si el primer pokemon del entrenador 1, puede vencer al ultimo pokemon del entrenador 2 no se contemplara en el resultado final.
-}
cantidadDePokemonesGanadoresDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
-- recibe un tipo de pokemon y dos listas de pokemones y retorna la cantidad de pokemones de la primera lista que le ganarian a los pokemones de la segunda, siguiendo como regla peleas 1 a 1.
cantidadDePokemonesGanadoresDeTipo tp _ [] = 0
cantidadDePokemonesGanadoresDeTipo tp [] _ = 0
cantidadDePokemonesGanadoresDeTipo tp (x:xs) (k:ks) = if mismoTipoDePokemon tp (tipoDePokemon x) && superaA x k 
                                                        then 1 + cantidadDePokemonesGanadoresDeTipo tp xs ks 
                                                        else cantidadDePokemonesGanadoresDeTipo tp xs ks

pokemonesDeEntrenador :: Entrenador -> [Pokemon]
pokemonesDeEntrenador (ConsEntrenador _ poke) = poke
-- funciones del tp 1 
mismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool
-- recibe dos tipos de pokemon e indica si son del mismo tipo.
mismoTipoDePokemon Agua Agua = True
mismoTipoDePokemon Planta Planta = True
mismoTipoDePokemon Fuego Fuego =True
mismoTipoDePokemon _ _ = False

tipoDePokemon :: Pokemon -> TipoDePokemon
-- retorna el tipo de pokemon de un pokemon
tipoDePokemon (ConsPokemon t _) = t

esTipoAgua :: TipoDePokemon -> Bool
-- indica si el tipo de pokemon es Agua
esTipoAgua Agua = True
esTipoAgua _ = False

esTipoFuego :: TipoDePokemon -> Bool
-- indica si el tipo de pokemon es Fuego
esTipoFuego Fuego = True
esTipoFuego _ = False

esTipoPlanta :: TipoDePokemon -> Bool
-- indica si el tipo de pokemon es Planta
esTipoPlanta Planta = True
esTipoPlanta _ = False

esPokemonTipoAgua :: Pokemon -> Bool
-- indica si el pokemon es de tipo Agua
esPokemonTipoAgua poke = esTipoAgua (tipoDePokemon poke)

esPokemonTipoFuego :: Pokemon -> Bool
-- indica si el pokemon es de tipo Fuego
esPokemonTipoFuego poke = esTipoFuego (tipoDePokemon poke)

esPokemonTipoPlanta :: Pokemon -> Bool
-- indica si el pokemon es de tipo Planta
esPokemonTipoPlanta poke = esTipoPlanta (tipoDePokemon poke)

superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon Agua _) (ConsPokemon Fuego _)   = True
superaA (ConsPokemon Fuego _) (ConsPokemon Planta _) = True
superaA (ConsPokemon Planta _) (ConsPokemon Agua _)  = True
superaA _ _                                          = False
-- variables de prueba
nicolasEntrenador = ConsEntrenador "nico" [bulbasaur, charmander, pokeTipoAgua]
alguienEntrenador = ConsEntrenador "nico" [charmander, pokeTipoAgua, bulbasaur]
bulbasaur = ConsPokemon Planta 40
charmander = ConsPokemon Fuego 90
pokeTipoAgua = ConsPokemon Agua 50

--3. Empleados IT
data Seniority = Junior | SemiSenior | Senior
    deriving (Show, Eq)
data Proyecto = ConsProyecto String
    deriving (Show, Eq)
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving (Show, Eq)
data Empresa = ConsEmpresa [Rol]
    deriving (Show, Eq)
--Variables de Prueba
empresa1 = ConsEmpresa [empleado1, empleado2, empleado3]
empleado1 = Developer Junior desarrolloDeApps
empleado2 = Management Senior desarrolloDeApps
empleado3 = Developer Senior analistaDeDatos
desarrolloDeApps = ConsProyecto "Desarrollo de Aplicaciones"
analistaDeDatos = ConsProyecto "Analista de Datos"
desarrolloDeApps2 = ConsProyecto "Desarrollo de Aplicaciones"
--Ejercicios
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa []) = [] 
proyectos (ConsEmpresa rs) = proyectoDeCadaRolDeLaEmpresa rs 

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer _ p)  = p
proyectoDeRol (Management _ p) = p

proyectoDeCadaRolDeLaEmpresa :: [Rol] -> [Proyecto]
-- recibe una lista de roles y retorna una lista de proyectos sin repetidos.
proyectoDeCadaRolDeLaEmpresa []     = []
proyectoDeCadaRolDeLaEmpresa (r:rs) =
                                if pertenece (proyectoDeRol r) (proyectoDeCadaRolDeLaEmpresa rs)
                                    then proyectoDeCadaRolDeLaEmpresa rs
                                    else proyectoDeRol r : proyectoDeCadaRolDeLaEmpresa rs

losDevSenior :: Empresa -> [Proyecto] -> Int
-- dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen además a los proyectos dados por parámetro.
losDevSenior (ConsEmpresa r) p = desarrolladoresSeniorQuePertenecenA r p

desarrolladoresSeniorQuePertenecenA :: [Rol] -> [Proyecto] -> Int
-- dada una lista de roles indica la cantidad de desarrolladores senior que ademas pertenece a al menos un opr
desarrolladoresSeniorQuePertenecenA []  _ = 0
desarrolladoresSeniorQuePertenecenA (r:rs) p = if esDesarrolladorSeniorYPerteneceA r p
                                                then 1 + desarrolladoresSeniorQuePertenecenA rs p
                                                else desarrolladoresSeniorQuePertenecenA rs p

esDesarrolladorSeniorYPerteneceA :: Rol -> [Proyecto] -> Bool
esDesarrolladorSeniorYPerteneceA _ [] = False
esDesarrolladorSeniorYPerteneceA r (p:ps) = if esDesarrolladorSenior r && perteneceA r p
                                                then True || esDesarrolladorSeniorYPerteneceA r ps
                                                else esDesarrolladorSeniorYPerteneceA r ps

perteneceA :: Rol -> Proyecto -> Bool
-- recibe un rol e indica si el rol pertenece al proyecto dado.
perteneceA r p2  = proyectoDeRol r == p2

desarrolladoresSeniorEn :: [Rol] -> [Rol]
-- dada una lista de roles retorna aquellos que son desarrolladores senior.
desarrolladoresSeniorEn   []   = []
desarrolladoresSeniorEn (r:rs) = if esDesarrolladorSenior r 
                                    then r : desarrolladoresSeniorEn rs
                                        else desarrolladoresSeniorEn rs

esDesarrolladorSenior :: Rol -> Bool
-- indica si un rol es desarrollador senior.
esDesarrolladorSenior (Developer Senior _) = True
esDesarrolladorSenior _ = False

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
-- Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn [] _ = 0
cantQueTrabajanEn ps (ConsEmpresa rs) = cantDeEnLosQueTrabajan ps rs

cantDeEnLosQueTrabajan :: [Proyecto] -> [Rol] -> Int
-- indica la cantidad de roles en los que trabajan en alguno de los proyectos dados.
cantDeEnLosQueTrabajan [] _ = 0
cantDeEnLosQueTrabajan _ [] = 0
cantDeEnLosQueTrabajan ps (r:rs) = if perteneceAlmenosAUn r ps
                                     then 1 + cantDeEnLosQueTrabajan ps rs
                                     else cantDeEnLosQueTrabajan ps rs

trabajaEn :: [Rol] -> Proyecto -> Bool
-- recibe una lista de roles y retorna la cantidad de roles que trabajan en el proyecto dado. 
trabajaEn [] _ = False
trabajaEn (r:rs) p = if perteneceA r p 
                        then True || trabajaEn rs p 
                        else trabajaEn rs p

perteneceAlmenosAUn :: Rol -> [Proyecto] -> Bool
-- recibe un rol e indica si el rol trabaja en al menos un proyecto de la lista dada.
perteneceAlmenosAUn _ [] = False
perteneceAlmenosAUn r (p:ps) = if perteneceA r p 
                                then True || perteneceAlmenosAUn r ps
                                else perteneceAlmenosAUn r ps

asignadosPorProyecto :: Empresa -> [(Proyecto,Int)]
-- Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su cantidad de personas involucradas.
asignadosPorProyecto e = listaDeTuplasDeEmpleadoYSuProyecto (proyectos e) (rolesDeLaEmpresa e)

rolesDeLaEmpresa :: Empresa -> [Rol]
rolesDeLaEmpresa (ConsEmpresa rs) = rs

listaDeTuplasDeEmpleadoYSuProyecto :: [Proyecto] -> [Rol] -> [(Proyecto,Int)] 
listaDeTuplasDeEmpleadoYSuProyecto [] _        = []
listaDeTuplasDeEmpleadoYSuProyecto _ []        = []
listaDeTuplasDeEmpleadoYSuProyecto (p : ps) rs = (p, cantEmpleadosPorProyecto p rs) : listaDeTuplasDeEmpleadoYSuProyecto ps rs 

cantEmpleadosPorProyecto :: Proyecto -> [Rol] -> Int
cantEmpleadosPorProyecto _ []       = 0
cantEmpleadosPorProyecto p (r : rs) = if perteneceA r p 
                                        then 1 + cantEmpleadosPorProyecto p rs 
                                        else cantEmpleadosPorProyecto p rs