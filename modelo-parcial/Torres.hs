
data Color = Azul | Verde | Rojo
data Torre = Base | Bloque Color Torre

cantidadDeBloques :: Torre -> Int
-- Propósito: dada una torre, indica la cantidad de bloques que contiene.
cantidadDeBloques Base         = 0
cantidadDeBloques (Bloque _ t) = 1 + cantidadDeBloques t

todos :: Color -> Torre -> Bool
-- Propósito: dados un color c y una torre, indica si todos los bloques de la torre son de color c.
todos c Base           = True
todos c1 (Bloque c2 t) = if esMismoColor c1 c2 
                            then True && todos c1 t
                            else False 

esMismoColor :: Color -> Color -> Bool
-- Proposito: dados dos colores indica si estos son iguales.
esMismoColor Azul Azul = True
esMismoColor Verde Verde = True
esMismoColor Rojo Rojo = True
esMismoColor _ _ = False

agregarOtrosN :: Int -> Color -> Torre -> Torre
-- Propósito: dados un número n, un color c y una torre, agrega n bloques de cemento de color c a la torre, luego del primer bloque de color c.
-- Precondición: hay al menos un bloque de color c.
agregarOtrosN 0 _ t              = t
agregarOtrosN n c Base           = 
agregarOtrosN n c1 (Bloque c2 t) = if esMismoColor c1 c2 
                                    then agregarBloque c 
sinColores :: [Color] -> Torre -> Torre
-- Propósito: dada una lista de colores y una torre, quita de la torre todos los bloques cuyo color sea alguno de los de la lista.
aparicionesDeColores :: Torre -> [(Color,Int)]
-- Propósito: dada una torre, denota la lista de pares donde el primer elemento es un color y el segundo elemento es la cantidad de veces que aparece dicho color.
-- Nota: si el color no aparece, no hace falta que esté en la lista.