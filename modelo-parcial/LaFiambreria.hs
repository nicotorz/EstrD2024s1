data Sanguche = Pan Relleno
    deriving Show
data Relleno = Feta TipoDeFeta Relleno | Aire
    deriving Show
data TipoDeFeta = Queso | Jamon | Mortadela | Salame
    deriving (Eq, Show) 

ejemplo1 :: Sanguche
ejemplo1 = Pan (Feta Queso Aire)
ejemplo2 :: Sanguche
ejemplo2 = Pan (Feta Jamon Aire)
ejemplo3 :: Sanguche
ejemplo3 = Pan (Feta Mortadela (Feta Salame Aire))
ejemplo4 :: Sanguche
ejemplo4 = Pan (Feta Queso (Feta Queso (Feta Salame (Feta Jamon (Feta Jamon (Feta Queso Aire))))))
ejemplo5 :: Sanguche
ejemplo5 = Pan Aire

-- Resolver las siguientes funciones utilizando recursión estructural:
rellenoDeAire :: Sanguche -> Bool
-- Propósito: Dado un sanguche, indica si el relleno es solo de aire.
rellenoDeAire (Pan relleno) = esRellenoDeAire relleno

esRellenoDeAire :: Relleno -> Bool
-- Proposito: Dado un relleno, indica si el relleno es solo de aire.
esRellenoDeAire Aire         = True
esRellenoDeAire (Feta tdf r) = False

esTortitaDeJamon :: Sanguche -> Bool
-- Propósito: Dado un sanguche indica si solo tiene fetas de jamon.
esTortitaDeJamon (Pan r) = esSoloRellenoDeJamon r

esSoloRellenoDeJamon :: Relleno -> Bool
-- Proposito: Dado un relleno indica si es solo de fetas de jamon.
esSoloRellenoDeJamon Aire         = True
esSoloRellenoDeJamon (Feta tdf r) = (esJamon tdf) && esSoloRellenoDeJamon r

esJamon :: TipoDeFeta -> Bool
-- Proposito: Dado un tipo de feta indica si es una feta de jamon.
esJamon Jamon = True
esJamon _     = False

mandaleNDe :: Int -> TipoDeFeta -> Sanguche -> Sanguche
-- Propósito: Dados un número n y un tipo de feta, agrega n fetas de ese tipo, al principio del relleno del sanguche dado.
mandaleNDe 0 _ s         = s
mandaleNDe n tdf (Pan r) = (Pan (agregarNFetas n tdf r))

agregarNFetas :: Int -> TipoDeFeta -> Relleno -> Relleno
-- Proposito: Dado un tipo de feta y un sanguche, denota el resultante de agregar ese tipo de feta en el sanguche.
agregarNFetas 0 tdf r = r
agregarNFetas n tdf r = Feta tdf (agregarNFetas (n-1) tdf r)

peroSinQueso :: Sanguche -> Sanguche
-- Propósito: Quita todo el queso del relleno al sanguche dado.
peroSinQueso (Pan r) = (Pan (rellenoSinQueso r))

rellenoSinQueso :: Relleno -> Relleno
-- Proposito: Quita todo el queso del relleno dado
rellenoSinQueso Aire         = Aire
rellenoSinQueso (Feta tdf r) = if esQueso tdf 
                                then rellenoSinQueso r 
                                else (Feta tdf (rellenoSinQueso r))

esQueso :: TipoDeFeta -> Bool
-- Proposito: Indica si el tipo de feta es queso
esQueso Queso = True
esQueso _     = False

ordenadosPorCantidad :: Sanguche -> [(TipoDeFeta, Int)]
-- Propósito: Devuelve una lista de fetas del relleno del sanguche, junto con su cantidad de apariciones.
ordenadosPorCantidad (Pan r) = ordenadosPorCantidadR r 

ordenadosPorCantidadR :: Relleno -> [(TipoDeFeta, Int)]
-- Propósito: Devuelve una lista de fetas del relleno dado, junto con su cantidad de apariciones.
ordenadosPorCantidadR Aire         = []
ordenadosPorCantidadR (Feta tdf r) = agregarATupla tdf (ordenadosPorCantidadR r)

agregarATupla :: TipoDeFeta -> [(TipoDeFeta, Int)] -> [(TipoDeFeta, Int)]
-- Recibe un tipo de feta y devuelve una lista de pares que representa los TipoDeFeta (sin repetir) junto con la cantidad de apariciones.
agregarATupla tdf []               = (tdf,1) : []
agregarATupla tdf1 ((tdf2,n):tdfs) = if (tdf1 == tdf2)  
                                        then (tdf2, n+1) : tdfs
                                        else (tdf2,n) : agregarATupla tdf1 tdfs
  

bajaDeTripulante :: Tripulante -> Nave -> Nave
-- Proposito: Elimina al tripulante de la nave
bajaDeTripulante t n = agregarTripulantesSinT (set2List (triplantes n)) t n 

agregarTripulantesSinT :: [Tripulante] -> Tripulante -> Nave -> Nave
-- Proposito: Dada una lista de tripulantes y una nave, devuelve la nave resultante de rearmar la nave sin el tripulante dado.
agregarTripulantesSinT [] _ n      = naveVacia sectores n 
agregarTripulantesSinT (t:ts) t2 n = if (t == t2) 
                                        then agregarTripulantesSinT ts t2 n
                                        else agregarTripulante t (sectorDe t n) (agregarTripulantesSinT ts t2 n)

naveVacia :: [Sector] -> Nave
naveVacia ss = MkN (sectoresVacios ss) emptyH (head ss, 0)

sectoresVacios :: [Sector] -> Map Sector (Set Tripulante) 
sectoresVacios [] = emptyM 
sectoresVacios (s:ss) = assocM s emptyS (sectoresVacios ss)