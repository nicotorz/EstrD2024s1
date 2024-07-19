#include "LinkedList.h"

int sumatoria(LinkedList xs) {
// Costo: O(N) siendo N la cantidad de elementos en xs.
// Proposito: Devuelve la suma de todos los elementos.
    ListIterator iterator = getIterator (xs);
    int sumatoria = 0;
    while (!atEnd (iterator)) {
        sumatoria += current (iterator);
        Next (iterator)
    }
    DisposeIterator (iterator);
    return sumatoria;
}

void Sucesores(LinkedList xs) {
// Incrementa en uno todos los elementos.
    ListIterator iterator = getIterator (xs);
    while (!atEnd (iterator)) {
        SetCurrent(current(xs)+1, iterator);
        Next (iterator);
    }
    DisposeIterator (iterator);
}

bool pertenece(int x, LinkedList xs) {
// Costo = O(N) como maximo siendo N la cantidad de elementos en xs.
// Indica si el elemento pertenece a la lista.
    ListIterator iterator = getIterator (xs);
    while (!atEnd (iterator)) {
        if (current(iterator) == x) {
            DisposeIterator (iterator);
            return true;
        }
    }
    DisposeIterator (iterator);
    return false;
}

int apariciones(int x, LinkedList xs) {
// Costo = O(N) siendo N la cantidad de elementos en xs.
// Indica la cantidad de elementos iguales a x.
    ListIterator iterator = getIterator (xs);
    int apariciones = 0;
    while (!atEnd (iterator)) {
        if (current(iterator) == x) {
            apariciones++;
        }
        next (iterator);
    }
    DisposeIterator (iterator);
    return apariciones;
}

int minimo(LinkedList xs) {
// Costo = O(N) siendo N la cantidad de elementos que hay en xs.
// Devuelve el elemento más chico de la lista.
    ListIterator iterator = getIterator(xs);
    int minimoE = current(iterator);
    while (!atEnd(iterator)) {
        if (current(iterator) < minimoE) {
            minimoE = current(iterator);
        }
        next (iterator);
    }
    DisposeIterator (iterator);
    return minimoE;
}

LinkedList copy(LinkedList xs) {
// Costo = O(N) debido a la memoria requerida para almacenar los N nodos de la nueva lista.
// Dada una lista genera otra con los mismos elementos, en el mismo orden.
// Nota: notar que el costo mejoraría si Snoc fuese O(1), ¾cómo podría serlo?// 
    ListIterator iterator   = getIterator(xs);
    LinkedList copyList = nil();
    while (!atEnd(iterator)) {
        Cons (current (iterator), copyList); 
        Next(iterator);
    }
    DisposeIterator (iterator);
    return copyList; 
}

void Append(LinkedList xs, LinkedList ys) {
// Agrega todos los elementos de la segunda lista al final de los de la primera.
// La segunda lista se destruye.
// Nota: notar que el costo mejoraría si Snoc fuese O(1), ¾cómo podría serlo?
    ListIterator Iterator = getIterator (ys);
    while(!atEnd(Iterator)) {
        Snoc(current Iterator, xs);
        Next(Iterator);
    }
    DisposeIterator(Iterator);
    DestroyL (ys);
}

// Snoc puede ser de costo O(1) si se le agrega una referencia(puntero) al ultimo nodo de la lista en la estructura LinkedListSt.
void Snoc(int x, LinkedList xs) {
    NodoL* nuevoNodo = new NodoL;
    nuevoNodo->elem = x;
    nuevoNodo->siguiente = NULL;

    if (isEmpty(xs)) {
        xs->primero = nuevoNodo;
        xs->ultimo = nuevoNodo;
    } else {
        xs->ultimo->siguiente = nuevoNodo;
        xs->ultimo = nuevoNodo;
    }
    xs->cantidad++;
}
