module Empresa 
    (Empresa, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores, agregarSector, agregarEmpleado, 
    agregarASector, borrarEmpleado)
    where

type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)

{-
INV.REP: 
    * Los empleados presentes en "(Map CUIL Empleado)" Son los mismo empleados 
      tambien presentes en "(Map SectorId (Set Empleado))"

    * Los empleados presentes en "(Map SectorId (Set Empleado))" Son los mismos empleados
      tambien presentes en "(Map CUIL Empleado)"

    * Un empleado puede estar asignado a más de un sector, es decir un mismo empleado puede
      aparecer como valor de distintas claves "SectorId" del map "(Map SectorId (Set Empleado))"

    * No puede Exisir un empleado en el set de empleados de "(Map SectorId (Set Empleado))" que NO
      este registrado con su "cuil" en el map "(Map CUIL Empleado)"


-}

consEmpresa :: Empresa
-- Propósito: construye una empresa vacía.
-- Costo: O(1)
consEmpresa = (ConsE emptyM emptyM)

buscarPorCUIL :: CUIL -> Empresa -> Empleado
-- Propósito: devuelve el empleado con dicho CUIL.
-- Costo: O(log E)
-- Su costo es O(log E), por que "lookupM" es costo "O(log m)" donde "E" es sobre la cantidad
-- Total de empleados en el Map "(Map CUIL Empleado)"
buscarPorCUIL c (ConsE mse mce) = case lookupM c mce of
                                  Just x  -> x
                                  Nothing -> error "No se existe el CUIL dado" 

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
-- Propósito: indica los empleados que trabajan en un sector dado.
-- Su costo es O(log S + E) porque: 
-- "lookupM" tiene costo "O(log m)" sobre los Sectores.
-- "setToList" tiene costo O(E) sobre los Empleados
empleadosDelSector sID (ConsE sectorMap _) = losEmpleadosEn sID sectorMap

losEmpleadosEn ::  SectorId -> (Map SectorId (Set Empleado)) -> [Empleado]
losEmpleadosEn sid m = case lookupM sid m of
                       Just x  -> setToList x
                       Nothing -> error "No existe el SectorId dado" 

todosLosCUIL :: Empresa -> [CUIL]
-- Propósito: indica todos los CUIL de empleados de la empresa.
-- Su costo es "O(E)" por que "keys" tiene costo O(n), en este caso es sobre la cantidad de Empleados 
todosLosCUIL (ConsE _ empleadosMap) = keys empleadosMap

todosLosSectores :: Empresa -> [SectorId]
-- Propósito: indica todos los sectores de la empresa.
-- Costo: O(S) por que "keys" tiene costo O(n), en este caso es sobre la cantidad de Sectores.
todosLosSectores (ConsE sectorMap _) = keys sectorMap

agregarSector :: SectorId -> Empresa -> Empresa
-- Propósito: agrega un sector a la empresa, inicialmente sin empleados.
-- Su costo es "O(logS)" por que para la insercion con "assocM" el map tiene costo logaritmico en Sectores
-- y "emptyS" tiene costo O(1) por lo tanto es despreciable.
agregarSector sID (ConsE sectorMap empleadosMap) = ConsE (assocM sID emptyS) empleadosMap

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
-- Propósito: agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el CUIL dado.
-- Costo: calcular.
{-
Justificacion de costo:

  en fromJust: O(1)
  * Costo : O(1)
  ===========================

  en incorporarASectores: O(S log S)
  
  * incorporarASectores es costo O(S log S), ya que utiliza incorporarSector que es O(log S) y debido a
    la recursion queda O(S log S) sobre la cantidad de Sectores de la lista

  ===========================

  en agregarEmp: O( N(log S + log E) )
* lookupM tiene costo: O(log S) sobre la cantidad de sectores del Map "Map SectorId (Set Empleado)"
  
* addS tiene costo: O(log E) sobre la cantidad de Empleados dados en el set recibido del "lookupM" 
  ya que tiene costos logarítmicos para inserción.
  
* assocM tiene costo: O(log S) sobre la cantidad de sectores ya que tiene costos logarítmicos para inserción.
  
* al utilizar recursion sobre la cantidad de elementos de la lista de [SectorId] el costo queda: O( N(log S + log E) )
-}
agregarEmpleado sids c (ConsE mse mce) = let empleado = incorporarSector sids (consEmpleado c) in
                                         ConsE (agregarEmp sids empleado mse) (assocM c empleado mce)

agregarEmp :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarEmp [] e m         = m
agregarEmp (sid:sids) e m = let setEmpleados = fromJust (lookupM sid m) in
                            assocM sid (addS e setEmpleados) (agregarEmp sids e m)

fromJust :: Maybe a -> a
fromJust (Just x) = x 
fromJust Nothing = error "Vacio"   

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
-- Propósito: agrega un sector al empleado con dicho CUIL.
-- Costo: calcular.
agregarASector sid c (ConsE mse mce) = let empleado = fromJust(lookupM c mce) in
                                       ConsE (agregarEmpASector empleado sid mse) mce

agregarEmpASector :: Empleado -> SectorId -> Map SectorId (Set Empleado)
agregarEmpASector emp sid m = assocM s (addS emp fromJust(lookupM sid m)) m

borrarEmpleado :: CUIL -> Empresa -> Empresa
-- Propósito: elimina al empleado que posee dicho CUIL.
-- Costo: calcular


