#include "ArrayList.h"

ArrayList newArrayList(){
// Crea una lista con 0 elementos.
// Nota: empezar el array list con capacidad 16.
    ArrayListSt* al = new ArrayListSt;
    al->cantidad = 0; 
    al->capacidad = 16;
    al->elementos = 0;
    return al;
}
ArrayList newArrayListWith(int capacidad){
// Crea una lista con 0 elementos y una capacidad dada por parámetro.
    ArrayListSt* al = new ArrayListSt;
    al->cantidad = 0; 
    al->capacidad = capacidad;
    al->elementos = new int[capacidad];
    return al;
}
int lengthAL(ArrayList xs){
// Devuelve la cantidad de elementos existentes.
    return (xs->elementos);
}
int get(int i, ArrayList xs){
// Devuelve el iésimo elemento de la lista.
    return (xs->elementos[i-1]);
}
void set(int i, int x, ArrayList xs){
// Reemplaza el iésimo elemento por otro dado.
    xs->elementos[i] = x;
}
void resize(int capacidad, ArrayList xs){
// Decrementa o aumenta la capacidad del array.
// Nota: en caso de decrementarla, se pierden los elementos del final de la lista.
    if (capacidad < xs->capacidad) {
        int* temp = new int[capacidad];
        for (int i=0; i < capacidad; i++) {
            temp[i] = xs->elementos[i];
            
        }
        delete xs->elementos
        xs->capacidad = capacidad;
        xs->elementos = temp;
    } else {
        int* temp = new int[capacidad];
        for (int i=0; i < xs->capacidad; i++) {
            temp[i] = xs->elementos[i];
        }
        delete xs->elementos;
        xs->capacidad = capacidad;
        xs->elementos = temp;
    }
}

void add(int x, ArrayList xs) {
// Agrega un elemento al final de la lista.
    if (xs->cantidad > xs->capacidad)
    {
        resize(xs->capacidad+1, xs);
    }

    set(xs->cantidad, x, xs);
    xs->cantidad++;
}

void remove(ArrayList xs){
// Borra el último elemento de la lista.
    if (xs->cantidad > 0) {
        xs->cantidad--;
        if (xs->cantidad < xs->capacidad / 2) {
            resize(-1, xs);
        }
    }
}

bool pertenece (int x, ArrayList xs) {
    for (int i = 0; i < xs.cantidad; i++)
    {
        if (get(i, xs) == x) {
            return true
        }
    }
    return false
}