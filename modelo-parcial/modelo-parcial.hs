programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
-- Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas programaron juntas.
-- Precondicion: Las personas pasadas por parametro existen en el organizador.
-- Costo: costo de programasP1 es O(log P) ya que utiliza la funcion programasDe que tiene ese costo.
--        costo de programasP2 es O(log P) ya que utiliza la funcion programasDe que tiene ese costo.
--        costo de intersection es O(P log P) 
--        dado que las funciones actuan sobre la misma estructura el costo de programasEnComun sera dominada por la eficiencia de intersection
--          lo que resulta en costo O(P log P)
programasEnComun p1 p2 org = let programasP1  = programasDe org p1
                                 programasP2  = programasDe org p2
                             in intersection programasP1 programasP2 

esUnGranHacker :: Organizador -> Persona -> Bool
-- Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
unGranHacker org p = let listaDePorgramasDeP = setToList (programasDe org p)
                     in 

