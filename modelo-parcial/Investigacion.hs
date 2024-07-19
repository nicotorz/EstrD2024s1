module Investigacion 
    (Investigacion, comenzarInvestigacion, cantEvidenciaIngresada, evidenciaIngresada, nombresIngresados, casoCerrado,
     esSospechoso, posiblesInocentes, ingresarPersonas, ingresarEvidencia)
    where
data Investigacion = ConsI (Map Nombre Persona)
                           (Map Evidencia [Nombre])
                           (PriorityQueue Persona)
                            Int
{- INV.REP.: ConsI mnp mens pqp n  
        * Todos los nombres pertenecientes a mens se encuentran en la lista de mnp.
        * Todas las personas pertenecientes a mnp se encuentran en pqp y viceversa.
        * n no puede ser negativo, si no hay ninguna evidencia es 0.
-}
comenzarInvestigacion :: Investigacion
-- Propósito: crea una investigación sin datos.
-- Eficiencia: O(1)
comenzarInvestigacion =  (ConsI emptyM emptyM emptyPQ 0)

cantEvidenciaIngresada :: Investigacion -> Int
-- Propósito: devuelve la cantidad de eviencia ingresada.
-- Eficiencia: O(1)
cantEvidenciaIngresada (ConsI _ _ _ n) = n

evidenciaIngresada :: Investigacion -> [Evidencia]
-- Propósito: devuelve la evidencia ingresada.
-- Eficiencia: O(N) siendo N la cantidad de claves en mens.
evidenciaIngresada (ConsI _ mens _ _) = domM mens

nombresIngresados :: Investigacion -> [Nombre]
-- Propósito: devuelve los nombres de personas ingresadas.
-- Eficiencia: costo N en listaDePersonas por uso de listToPQ 
--               
nombresIngresados (ConsI _ _ pqp _) = 
    nombre (maxPQ pqp) : nombresIngresados (ConsI _ _ (deleteMaxPQ pqp) _)

casoCerrado :: Investigacion -> Bool
-- Propósito: indica si la investigación posee al menos una persona con 5 evidencias en su contra.
-- Eficiencia: O(1)
casoCerrado (ConsI _ _ pqp _) = if cantEvidencia (maxPQ pqp) >= 5
                                    then True
                                    else False

esSospechoso :: Nombre -> Investigacion -> Bool
-- Propósito: indica si esa persona tiene al menos una evidencia en su contra.
-- Nota: la persona puede no existir.
-- Eficiencia: O(log N)
esSospechoso n (ConsI mnp _ _ _) = case lookupM n mnp of
                                    Nothing -> False
                                    Just p  -> esSospechosoP p

esSospechosoP :: Persona -> Bool
-- Proposito: indica si la persona dada tiene al menos una evidencia en su contra 
-- Eficiencia: 
esSospechosoP p = cantEvidencia p > 0   

posiblesInocentes :: Investigacion -> [Persona]
-- Propósito: devuelve a las personas con cero evidencia en su contra.
-- Eficiencia: O(N log N)
posiblesInocentes (ConsI _ _ pqp _) = let personasEnI = listToPQ pqp in
                                      posiblesInocentesL personasEnI

posiblesInocentesL :: [Persona] -> [Persona]
-- Proposito: dada una lista de personas devuelve aquellas que son presuntos inocentes
-- Eficiencia: O(N) siendo N la cantidad de personas la lista
posiblesInocentesL []     = []
posiblesInocentesL (p:ps) = if cantEvidencia p == 0
                                then p : posiblesInocentesL ps
                                else posiblesInocentesL ps 

ingresarPersonas :: [Nombre] -> Investigacion -> Investigacion
-- Propósito: ingresa a personas nuevas a la investigación (mediante sus nombres), sin evidencia en su contra.
-- Precondición: las personas no existen en la investigación y no hay nombres repetidos.
-- Eficiencia: O(N log N)
ingresarPersonas ns (ConsI mnp mens pqp n) = (ConsI (agregarPSM ns mnp) mens (agregarPSPQ ns pqp) n)

agregarPSM :: [Nombre] -> Map Nombre Persona -> Map Nombre Persona
-- Precondicion: ingresa a las personas en el map
agregarPSM []     mnp = mnp
agregarPSM (n:ns) mnp = assocM n (crearP n) agregarPSM ns mnp

agregarPSPQ :: [Nombre] -> PriorityQueue Persona -> PriorityQueue Persona
-- Precondicion: ingresa a las personas en la pq
agregarPSPQ []     pqp = pqp
agregarPSPQ (n:ns) pqp = insertPQ (crearP n) agregarPSPQ ns pqp

ingresarEvidencia :: Evidencia -> Nombre -> Investigacion -> Investigacion
-- Propósito: asocia una evidencia a una persona dada.
-- Precondición: la evidencia aún no está asociada a esa persona.
-- Nota: la persona y la evidencia existen, pero NO están asociadas.
-- Eficiencia: O(N log N)
ingresarEvidencia e n (ConsI mnp mens pqp n) = 
    (ConsI (agregarEvidenciaMNP e n mnp) (agregarEvidenciaMENS e n mens) (insertarEvidenciaPQ e n pq) n+1)

agregarEvidenciaMNP :: Evidencia -> Nombre -> Map Nombre Persona -> Map Nombre Persona
-- Proposito: asocia una evidencia a una persona dada en el map.
agregarEvidenciaMNP e n mnp = case lookupM n mnp of 
                                Nothing -> mnp 
                                Just p  -> assocM n (agregarEvidencia e p) mnp

agregarEvidenciaMENS :: Evidencia -> Nombre -> Map Evidencia [Nombre] -> Map Evidencia [Nombre]
-- Proposito: asocia una evidencia a la persona dada en el Map.
-- Precondicion: la evidencia esta asosiada al Map.
agregarEvidenciaMENS e n mens = assocM e (fromJust ((lookupM e):n)) mens 

insertarEvidenciaPQ :: Evidencia -> Nombre -> PriorityQueue Persona -> PriorityQueue Persona
-- Proposito: asocia una evidencia a la persona dada en la pq
insertarEvidenciaPQ n s pq = let nn = maxPQ pq in
                                if (nombre nn) == n
                                    then insertPQ (agregarEvidencia e nn) (deleteMaxPQ)
                                    else insertPQ nn (insertarEvidenciaEn e n (deleteMaxPQ pq))

-- Usuario
-- Implementar las siguientes funciones como usuario del tipo Investigacion, indicando el costo obtenido:
comenzarConPersonas :: [Nombre] -> Investigacion
-- Propósito: Comienza una investigación con una lista de nombres sin evidencia.
comenzarConPersonas ns = ingresarPersonas ns comenzarInvestigacion 

todosInocentes :: Investigacion -> Bool
-- Propósito: Indica si las personas en la investigación son todas inocentes.
todosInocentes inv = todosInocentes (nombresIngresados inv) inv

todosInocentesL :: [Nombre] -> Investigacion -> Bool
-- Proposito: Dado una lista de nombres y una investifacion, indica si no hay sospechosos relacionados algun nombre de la lista.
todosInocentesL [] inv      = True
todosInocentesL (n:ns) inv  = if esSospechoso n inv 
                                then False 
                                else True && todosInocentesL ns inv

terminaCerrado :: [(Evidencia, Nombre)] -> Investigacion -> Bool
-- Propósito: Indica si la evidencia en la lista es suficiente para cerrar el caso.
terminaCerrado [] inv          = casoCerrado inv
terminaCerrado ((e,n):ens) inv = terminaCerrado ens (ingresarEvidencia e n inv) 
