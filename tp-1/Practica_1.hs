--2. NUMEROS ENTEROS
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

--3. TIPOS ENUMERATIVOS
-- 1
data Dir = Norte | Este | Sur | Oeste
    deriving Show
-- 1.a
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este
--1.b
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales d1 d2 = False
--1.c
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "La direccion Oeste no posee una siguiente direccion"
    --PREC: No se puede tomar como parametro la Direccion Oeste ya que no posee una siguiente direccion.
{-
    ¿Es una función total o parcial? ¿Por qué?
    · Es una funcion parcial por que no esta definida para todos los valores posibles de tipo Dir, ya que Oeste no posee una siguiente direccion.
-}

--2
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show
--2.a 
primerYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primerYUltimoDia = (Lunes, Domingo)
--2.b
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM unDia = False
--2.c
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes Martes = True
vieneDespues Martes Miercoles = True
vieneDespues Miercoles Jueves = True
vieneDespues Jueves Viernes = True
vieneDespues Viernes Sabado = True
vieneDespues Sabado Domingo = True
vieneDespues Domingo Lunes = True
vieneDespues unDia1 unDia2 = False
--2.d
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True
{- otra opcion
    noEsLunes :: DiaDeSemana -> Bool
    noEsLunes Lunes = False
    noEsLunes _ = True 
    noEsDomingo :: DiaDeSemana -> Bool
    noEsDomingo Domingo = False
    noEsDomingo _ = True
    estaEnElMedio :: DiaDeSemana -> Bool
    estaEnElMedio d = (noEsLunes d) && (noEsDomingo d)
-}

--3.a
negar :: Bool -> Bool
negar True = False
negar False = True
--3.b
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True
--3.c
yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False
--3.d
oBien :: Bool -> Bool -> Bool
oBien False False = False
oBien _ _ = True

--4. REGISTROS
--1.Persona
data Persona = P String Int
               --Nombre Edad
    deriving Show
    
nombre :: Persona -> String 
nombre (P n e) = n
edad :: Persona -> Int
edad (P n e) = e
crecer :: Persona -> Persona
crecer (P n e) = P n (e+1)
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nNuevo (P n e) = P nNuevo e
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = 
    if edad p1 > edad p2
        then p1
        else p2
{- Variables de prueba
    nico = P "Nicolas" 24
    ramon = P "Ramon" 20 
-}     
--2.Pokemon
data TipoDePokemon = Agua | Fuego | Planta
    deriving Show
data Pokemon = Poke TipoDePokemon Int
                --Porcentaje De Energia
    deriving Show
data Entrenador = E String Pokemon Pokemon
                  --Nombre
    deriving Show

tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (Poke t _) = t
esTipoAgua :: TipoDePokemon -> Bool
esTipoAgua Agua = True
esTipoAgua _ = False
esTipoFuego :: TipoDePokemon -> Bool
esTipoFuego Fuego = True
esTipoFuego _ = False
esTipoPlanta :: TipoDePokemon -> Bool
esTipoPlanta Planta = True
esTipoPlanta _ = False
esPokemonTipoAgua :: Pokemon -> Bool
esPokemonTipoAgua poke = esTipoAgua (tipoDePokemon poke)
esPokemonTipoFuego :: Pokemon -> Bool
esPokemonTipoFuego poke = esTipoFuego (tipoDePokemon poke)
esPokemonTipoPlanta :: Pokemon -> Bool
esPokemonTipoPlanta poke = esTipoPlanta (tipoDePokemon poke)
superaA :: Pokemon -> Pokemon -> Bool
superaA poke1 poke2 = (esPokemonTipoAgua poke1) && (esPokemonTipoFuego poke2)
                    || (esPokemonTipoFuego poke1) && (esPokemonTipoPlanta poke2)
                    || (esPokemonTipoPlanta poke1) && (esPokemonTipoAgua poke2)
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tp (E _ poke1 poke2) =
                                    if mismoTipoDePokemon (tipoDePokemon poke1) tp && mismoTipoDePokemon (tipoDePokemon poke2) tp
                                        then 2
                                        else if mismoTipoDePokemon (tipoDePokemon poke1) tp || mismoTipoDePokemon (tipoDePokemon poke2) tp
                                            then 1
                                            else 0
mismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool
mismoTipoDePokemon Agua Agua = True
mismoTipoDePokemon Planta Planta = True
mismoTipoDePokemon Fuego Fuego =True
mismoTipoDePokemon _ _ = False
listaDePokemonesDe :: Entrenador -> [Pokemon]
listaDePokemonesDe (E _ poke1 poke2) = [poke1, poke2]
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = listaDePokemonesDe e1 ++ listaDePokemonesDe e2
{- Variables de prueba
    pokemonAgua = Poke Agua 10
    pokemonFuego = Poke Fuego 10
    pokemonPlanta = Poke Planta 20
    entrenadorNico = E "Nicolas" pokemonAgua pokemonAgua
    entrenadorJuan = E "Juan" pokemonPlanta pokemonPlanta
-}

--5. FUNCIONES POLIMORFICAS
--1.a
loMismo :: a -> a
loMismo a = a
--1.b
siempreSiete :: a -> Int
siempreSiete a = 7 
--1.c
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
{- ¿Por qué existen dos variables de tipo diferentes?
    La existencia de dos variables de tipos diferentes permite que se pueda trabajar con funciones
    y estructuras con diferentes tipos de datos, esto hace que no este limitada a un solo tipo de dato.

    2. Responda la siguiente pregunta: ¿Por qué estas funciones son polimórficas?
    Estas funciones son polimorficas por que trabajan sobre tipos de datos genericos y no estan restrigindas por un solo tipo de dato.
-}
--6. PATTERN MATCHING sobre listas
--6.2
estaVacia :: [a] -> Bool
estaVacia [] = True 
estaVacia _  = False
--6.3
elPrimero :: [a] -> a
elPrimero (a:_) = a
    --PREC: La lista no debe ser vacia.
--6.4
sinElPrimero :: [a] -> [a]
sinElPrimero (_:bs) = bs
    --PREC: La lista no debe ser vacia.
--6.5
splitHead :: [a] -> (a, [a])
splitHead (x:y) = ( elPrimero (x:y), sinElPrimero (x:y) )
    --PREC: La lista no debe ser vacia.
