module Torneo 
    (Torneo, equipo, equipoDe, jugadores, equipoGoleador, comenzarTorneo, equipos, registrarGol, ingresarJugador,
    sinEquipoGoleador)
    where 

import Map
import PriorityQueue 

type Nombre = String 
data Torneo =  ConsT (Map Nombre Equipo)
                     (Map Nombre Nombre)
                     (PriorityQueue Equipo)

    {- INV.REP.: 
        * Todos los nombres de equipos que pertenecen al Map Nombre Equipo, deben pertenecer a Map Nombre Nombre y a la PQ Equipo.
        * Todos los equipos que pertencen a la PriorityQueue Equipo deben pertenecer al Map Nombre Equipo y viceversa.
        * No puede existir un Nombre de jugador que no pertenezca al Equipo relacionado con el Nombre en el Map Nombre Nombre.
    -}
-- IMPLEMENTADOR 

equipo :: Nombre -> Torneo -> Maybe Equipo
-- Propósito: dado un nombre de equipo devuelve al equipo con dicho nombre.
-- Eficiencia: O(log N) por que se usa la funcion lookupM sobre el Map Nombre Equipo
equipo n (ConsT mne _ _) = lookupM n mne

equipoDe :: Nombre -> Torneo -> Maybe Equipo
-- Propósito: dado un nombre de jugador devuelve el equipo en el que juega.
-- Nota: el jugador puede no existir, pero si existe, su equipo también.
-- Eficiencia: O(log N) ya que se hace lookupM sobre el Map que relaciona nombres de jugadores con equipos, y luego se vuelve a utilizar sobre 
--            la misma cantidad de nombres de equipos por lo tanto Costo: log N + log N = log N
equipoDe n (ConsT mne mnn _) = case lookupM n mnn of
                               Nothing     -> Nothing
                               Just nombre -> lookupM nombre mne

jugadores :: Torneo -> [Nombre]
-- Propósito: denota la lista de jugadores del torneo.
-- Eficiencia: O(N) ya que se utiliza la funcion domM que tiene costo lineal siendo N los Nombres de los jugadores 
jugadores (ConsT _ mnn _) = domM mnn 

equipoGoleador :: Torneo -> Equipo
-- Propósito: indica el equipo que más goles anotó.
-- Precondición: existe al menos un equipo en el torneo.
-- Eficiencia: O(1) por que se utiliza la funcion maxPQ de costo constante sobre los equipos en la PQ 
equipoGoleador (ConsT _ _ pe) = maxPQ pe 

comenzarTorneo :: [Equipo] -> Torneo
-- Propósito: devuelve un torneo en el que participan los equipos dados.
-- Nota: los equipos ya poseen jugadores, no olvidar sumarlos a la estructura.
-- Eficiencia: O(N2)
comenzarTorneo equipos = (ConsT (asociarEquiposATorneo equipos) (asociarJugadoresConEquipos equipos) listToPQ equipos)

asociarEquiposATorneo :: [Equipo] -> Map Nombre Equipo 
-- Proposito: dada una lista de equipos devuelve un torneo con los equipos asociados a su nombre unicamente.
-- Eficicencia: 
asociarEquiposATorneo []     = emptyM 
asociarEquiposATorneo (e:es) = assocM (nombre e) e (asociarEquiposATorneo es)

asociarJugadoresConEquipos :: [Equipo] -> Map Nombre Nombre 
-- Proposito: dada una lista de equipos devuelve un torneo con los nombres de los jugadores asociados al nombre del equipo que pertenece
-- Eficiencia: 
asociarJugadoresConEquipos []     = emptyM
asociarJugadoresConEquipos (e:es) = unionM (asociarTodosLosJugadores e) (asociarJugadoresConEquipos es)   

asociarTodosLosJugadores :: Equipo -> Map Nombre Nombre 
-- Proposito: dado un equipo asocia todos sus jugadores en un Map con el nombre de jugador como clave y el nombre del equipo como valor
-- Eficiencia: 
asociarTodosLosJugadores e = asociarNombresConEquipo (jugadores e) (nombre e)

asociarNombresConEquipo :: [Nombre] -> Nombre -> Map Nombre Nombre 
-- Proposito: dado una lista de nombres y un nombre de equipo asocia en un map a todos los jugadores con el nombre del equipo dado.
asociarNombresConEquipo [] _      = emptyM
asociarNombresConEquipo (n:ns) ne = assocM n ne (asociarNombresConEquipo ns ne) 

listToPQ :: [a] -> PriorityQueue a
-- Proposito: dada una lista transforma esa lista en una PQ
listToPQ []     = emptyPQ
listToPQ (a:as) = insertPQ a (listToPQ as)

equipos :: Torneo -> [Equipo]
-- Propósito: denota la lista de equipos del torneo.
-- Eficiencia: O(N log N) siendo N la cantidad de equipos en mne, ya que se utiliza la funcion equiposL con costo N log N .
equipos (ConsT mne _ _) = equiposL (domM mne) mne 

equiposL :: [Nombre] -> Map Nombre Equipo -> [Equipo]
-- Proposito: Dada una lista de nombres denota todos los equipos que estan asociados con dicho nombre.
-- Precondicion: los Nombres que estan en la lista deben estar relacionados con un equipo en el map.
-- Eficiencia: O(N log N) ya que se utiliza la funcion lookupM que tiene costo log N siendo N la cantidad de nombres de equipo.
--             y se itera sobre la cantidad de nombres en la lista, teniendo un costo de N siendo N la cantidad de nombres de equipo.
--             por lo tanto el costo es N * log N = N log N
equiposL [] _       = []
equiposL (n:ns) mne = fromJust (lookupM n mne) : equiposL ns mne 

fromJust :: Maybe a -> a
-- Proposito: Denota el valor contenido en un maybe
-- Precondicion: El Maybe no puede ser Nothing
-- Costo O(1)
fromJust (Just a) = a
fromJust Nothing  = error "no cumple con la precondicion"

registrarGol :: Nombre -> Nombre -> Torneo -> Torneo
-- Propósito: dados un nombre de jugador y un nombre de equipo, ingresa un gol anotado por el jugador dado para el equipo dado.
-- Precondición: existe un jugador y un equipo con dichos nombres.
-- Eficiencia: O(N log N)
registrarGol nj ne (ConsT mne mnn pqe) = ConsT (registrarGolM nj ne mne) mnn (registrarGolPQ nj ne pqe)

registrarGolM :: Nombre -> Nombre -> Map Nombre Equipo -> Map Nombre Equipo
-- Proposito: dados un nombre de jugador y un nombre de equipo, ingresa un gol anotado por el jugador dado para el equipo dado si el equipo
--            se encuentra en el Map dado, de lo contrario no hace nada.
registrarGolM nj ne mne = case lookupM ne of 
                            Just equipo -> assocM ne (anotarGol nj equipo) mne 
                            Nothing     -> mne 

registrarGolPQ :: Nombre -> Nombre -> PriorityQueue Equipo -> PriorityQueue Equipo
-- Propósito: Dados un nombre de jugador y un nombre de equipo, ingresa un gol anotado por el jugador dado para el equipo dado.
registrarGolPQ _ _ emptyPQ = emptyPQ  
registrarGolPQ nj ne pqe =
    let equipos = toList pqe  -- Se pasa la pq a una lista regular 
        equiposModificados = modificarEquipo nj ne equipos  -- Se modifican los equipos de la lista 
    in listToPQ equiposModificados  -- Se vuelve a pasar la lista regular a pq 

modificarEquipo :: Nombre -> Nombre -> [Equipo] -> [Equipo]
-- Proposito: Modifica un equipo si es el equipo al que pertenece el gol.
modificarEquipo _ _ []       = []
modificarEquipo ne nj (e:es) = if ne == (nombre e)
                                then anotarGol ne e : es
                                else e : modificarEquipo es

ingresarJugador :: Nombre -> Nombre -> Torneo -> Torneo
-- Propósito: dado un nombre de jugador y un nombre de equipo, ingresa al torneo dicho jugador, con cero goles, agregándolo al equipo dado.
-- Eficiencia: O(N log N)
ingresarJugador nj ne (ConsT mne mnn pqe) = (ConsT (ficharJugador nj ne mne) (assocM nj ne mnn) ficharJugadorPQ nj ne pqe)

ficharJugador :: Nombre -> Nombre -> Map Nombre Equipo -> Map Nombre Equipo
-- Proposito: dado un nombre de jugador y un nombre de equipo, ficha a ese jugador en el equipo perteneciente, de no haber equipos con ese nombre 
--            no hace nada.
-- Eficiencia: 
ficharJugador nj ne mne = case lookupM ne mne of
                            Just equipo -> assocM ne (fichar nj equipo) mne 
                            Nothing     -> mne 

ficharJugadorPQ :: Nombre -> Nombre -> PriorityQueue Equipo -> PriorityQueue Equipo
-- Propósito: Dados un nombre de jugador y un nombre de equipo, ingresa un gol anotado por el jugador dado para el equipo dado.
ficharJugadorPQ _ _ emptyPQ = emptyPQ  -- Si la cola de prioridad está vacía, devuelve una cola de prioridad vacía.
ficharJugadorPQ nj ne pqe =
    let equipos = toList pqe  -- Convertimos la cola de prioridad en una lista de equipos
        equipoModificado = ficharJugadorL nj ne equipos  -- Modificamos cada equipo en la lista
    in listToPQ equipoModificado  -- Convertimos la lista modificada en una nueva cola de prioridad

ficharJugadorL :: Nombre -> Nombre -> [Equipo] -> [Equipo]
-- Proposito: Ficha a un jugador al equipo perteneciente
ficharJugadorL _ _ []       = []
ficharJugadorL ne nj (e:es) = if ne == (nombre e)
                                then fichar nj ne e : es
                                else e : ficharJugadorL es

sinEquipoGoleador :: Torneo -> Torneo
-- Propósito: devuelve un torneo donde se ha quitado al equipo con más goles anotados.
-- Eficiencia: O(N log N)
sinEquipoGoleador (ConsT mne mnn pqe) = let equipoGoleador = maxPQ pqe in
                                        (ConsT (deleteM (nombre equipoGoleador) mne) (sinJugadoresDeEquipo equipoGoleador mnn) deleteMaxPQ pqe) 

sinJugadoresDeEquipo :: Equipo -> Map Nombre Nombre -> Map Nombre Nombre 
-- Proposito: devuelve un map de nombres de jugadores y nombres de equipos sin los jugadores del equipo dado.
sinJugadoresDeEquipo e mnn = let jugadores = domM mnn in 
                             sinJugadoresDe jugadores e mnn 

sinLosJugadoresDe :: [Nombre] -> Equipo -> Map Nombre Nombre -> Map Nombre Nombre 
-- Proposito: devuelve un map de nombre de jugadores y nombres de equipos sin los jugadores dados que pertenezcan al equipo dado.
sinLosJugadoresDe [] _ mnn     = mnn 
sinLosJugadoresDe (n:ns) e mnn = if (lookupM n mnn == Just (nombre e))
                                    then deleteM n (sinLosJugadoresDe ns e mnn)
                                    else (sinLosJugadoresDe ns e mnn)

-- USUARIO 

anotarGoles :: [(Nombre, Nombre)] -> Torneo -> Torneo
-- Propósito: dada una lista de pares de nombre de jugador (primera componente) y nombre de equipo (segunda componente), 
--            anota un gol en el torneo por cada elemento en la lista.
anotarGoles [] t             = t 
anotarGoles [(nj,ne):njes] t = anotarGoles njes (registrarGol nj ne t)

mejoresEquipos :: Int -> Torneo -> [Equipo]
-- Propósito: dado un número n denota a los n equipos más goleadores, ordenados por cantidad de goles de mayor a menor.
-- Precondición: existen al menos n equipos en el torneo.
-- Eficiencia: O(N log N)
mejoresEquipos 0 t = []
mejoresEquipos n t = equipoGoleador t : mejoresEquipos (n-1) (sinEquipoGoleador t)

jugadoresYGoles :: Torneo -> [(Nombre, Int)]
-- Propósito: denota la lista de jugadores del torneo junto con sus respectivos goles.
-- Eficiencia: O(N log N)
jugadoresYGoles t = let jugadoresT = jugadores t in
                    jugadorYGolesL jugadoresT t

jugadorYGolesL :: [Nombre] -> Torneo -> [(Nombre, Int)]                    
-- Propósito: denota la lista de jugadores del torneo junto con sus respectivos goles.
jugadorYGolesL [] _     = []
jugadorYGolesL (n:ns) t = (n, (golesDe n (equipoDeJugador n t))) : jugadorYGolesL ns t 

equipoDeJugador :: Nombre -> Torneo -> Equipo
-- Proposito: denota el equipo al que pertenece el jugador.
-- Precondicion: el jugador debe pertenecer al torneo.
equipoDeJugador n t = fromJust (equipoDe n t)


mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps map1 map2 = agregarClaves (keys map1) map1 map2

agregarClaves :: Eq k => [k] -> Map k v -> Map k v -> Map k v
agregarClaves [] _ map         = map
agregarClaves (k:ks) map1 map2 = case lookupM k map1 of    
                                    Just v  -> agregarClaves ks map1 (assocM k v map2)
                                    Nothing -> agregarClaves ks map1 map2


unionM :: Eq k => Map k v -> Map k v -> Map k v
unionM map1 map2 = agregarClaves (keys map1) map1 map2

agregarClaves :: Eq k => [k] -> Map k v -> Map k v -> Map k v
agregarClaves [] _ map         = map
agregarClaves (k:ks) map1 map2 = case lookupM k map2 of    
                                    Just _  -> agregarClaves ks map1 map2         
                                    Nothing -> agregarClaves ks map1 (assocM k (lookupM k map1) map2)