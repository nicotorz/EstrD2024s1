## Parcial - DoubleMap

Un diccionario doble es un diccionario que tiene 2 claves pero que permite obtener todos los valores asociados a 1 sola dichas claves por consulta. Asi, se pueden pedir todos los valores asociados a un caso particular de la primera clave o todos los valores asociados a un caso particular de la segunda clave, pero no de ambas al mismo tiempo. Se pdoria pensar como un matriz en la que queremos acceder a todos los valores de una fila o de una columna de manera eficiente.

Se puede modelar con un TAD DoubleMap k v, cuya interfaz es la que se enuncia a continuacion. Se tulizan los valores K1 para la cantidad de claves que aparecen como primera clave, K2 para la cantidad de claves que aparecen como segunda clave y K12 para la cantidad maxima de primeras claves asociadas a una segunda clave 

`emptyCM :: DoubleMap k v`
Describe un doble map vacio 
Eficiencia: O(1)
`assocCM :: Eq k => k -> k -> v -> DoubleMap k v -> DoubleMap k v`
Describe el resultado de modificar el map dado, agregando el valor a las dos claves dadas, en el orden dado.
Esto es, en assocCM k1 k2 v m se asocia como valor v a la clave k1 como primera clave y a la clave k2 como segunda clave, reemplazando el valor anterior asociado a ambas claves, si lo hubiese.
Eficiencia: O(log K1 + log K2)
`lookupCM :: Eq k => Either k k -> DoubleMap k v -> [ (k,v) ]`
Describe la busqueda por clave en un doble map.
NOTA el tipo Eiter a b se defino como
    data Either a b = Left a | Right b
y se utiliza para distinguir el origen de los datos desde dos fuentes diferentes. En este caso los valores de la forma (Left k1) indican que la clave a buscar es la primera y deben devolverse todos los valores asociados, organizados segun la segunda clave, mientras que los valores de la forma (Right K2) indican que la calve a buscar es la segunda y deben devolverse todos los valores asociados segun la primera clave
Eficiencia: O(max(log K1 + K2 log K2) (log K2 + K1 log K1))
`keysCM :: DoubleMap k v -> ([k],[k])`
Describe el conjunto de claves que son primera clave, y el conjunto de claves que son segunda clave.

### Ejercicios
1. En este ejercicio se utiliza una instancia de DoubleMap para representar los datos de docentes y estudiantes calificando mutuamente su desempeño. La primera componente es la clasificacion del estudiante sobre el docente y la segunda la del docente sobre el estudiante. Se define
    
    ** type Nombre = String  **
    ** type Calificaciones = DoubleMap Nombre [Int, Int]**
    donde los strings representan los nombres de los docentes y estudiantes y el par de enteros tiene primero la calificacion del docente al estudiante y luego la del estudiante al docente. Implementar como usuario las siguientes operaciones.

    a. `docentesCalificados :: Calificaciones -> [Nombre]`
    Describe la lista de docentes que recibieron una calificacion.
    Eficiencia: O (K1 + K2)

    b. `topNMasAfines :: Calificaciones -> Nombre -> Int [(Int, Nombre)]`
    Dado un estudiante y un numero n. describe kla lista de los n docentes que tienen mas afinidad con el, junto con el valor de esas afinidades. Eciste una funcion
        `afinidad :: [Int, Int] -> Int`
    que calcula la afinidad entre dos calificaciones reciprocas (la que el docente dio al estudiante y la que el estudiante dio al docente)
    AYUDA: utilizar una heap para almacenar los nombres de los docentes calificados por el estudiante dado ordenados por afinidad.
    Eficiencia: O((N + K12) log K12 + log K2 + K1 log K1), siendo N el valor del numero pedido.

2. Para implementar este TAD se decide el siguiente tipo de implementacion.
    data DoubleMap k v =
     DM (Map k (Map k v)) -- Calif. por clave 1, organizadas por clave 2
        (Map k (Map k v)) -- Calif. por clave 2, organizadas por clave 1 
donde se utilizan 2 maps simples para guardar en el primero los valores asociados a la 1ra clave dentro de un.....

## Anexo de interfaces

### Map
- `emptyM -> Map k v` **O(1)**
- `lookupM -> k -> Map k v -> Maybe v` **O(Log N)**
- `assocM -> k -> v -> Map k v -> Map k v` **O(Log N)**
- `deleteM -> k -> Map k v -> Map k v` **O(Log N)**
- `keys -> Map k v -> [k]` **O(N)**
- `sizeM -> Map k v -> Int` **O(1)**
