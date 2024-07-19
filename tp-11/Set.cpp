#include <iostream>
#include "LinkedList.h"
#include "Set.h"
#include "LinkedList.cpp"
using namespace std;

struct NodoS {
int elem; // valor del nodo
NodoS* siguiente; // puntero al siguiente nodo
}
struct SetSt {
int cantidad; // cantidad de elementos diferentes
NodoS* primero; // puntero al primer nodo
}
typedef SetSt* Set;

// Defniir la siguiente interfaz de este tipo de conjuntos, indicando el costo obtenido (intentar que sea lo más eficiente posible): 
Set emptyS() {
// Costo: O(1).
// Proposito:Crea un conjunto vacío.
    SetSt* s = new SetSt;
    s->cantidad = 0;
    s->primero = NULL;
    return s;
}

bool isEmptyS(Set s) { 
// Costo: O(1).
// Proposito:Indica si el conjunto está vacío.
    return (s->cantidad == 0);
}

bool belongsS(int x, Set s) {
// Costo: O(N) como maximo siendo N la cantidad de elementos en s.
// Proposito:Indica si el elemento pertenece al conjunto.
    NodoS* nodoActual = s->primero;
    while (nodoActual != NULL) {
        if (nodoActual->elem == x) {
            return true;
        }
        nodoActual = nodoActual->siguiente;
    }
    return (false);
}

void AddS(int x, Set s) {
// Costo: O(N) por uso de belongsS siendo N la cantidad de elementos en s, si x pertenece al set s
//        O(N+1) por uso de isEmpty s, O(N+1+N) por iteracion, siendo N la cantidad de elementos en S,
//        como N son lo mismo el Costo resultante como maximo es O(N).
// Proposito:Agrega un elemento al conjunto.
    if (belongsS (x,s)) {
        return; // no hacer nada, no se quiere agregar un elemento repetido
    } 
    NodoS* nodoAAgregar = new NodoS;
    nodoAAgregar->elem = x;
    nodoAAgregar->siguiente = NULL;

    if (isEmptyS (s)) {
        s->primero = nodoAAgregar;
    } else {
        NodoS* nodoActual = s->primero
        while (nodoActual->siguiente != NULL) {
            nodoActual = nodoAConsultar->siguiente;
        }
        nodoActual->siguiente = nodoAAgregar;
    }
    s->cantidad++;

}

void RemoveS(int x, Set s) {
// Costo: O(N), donde N es la cantidad de elementos en el conjunto s. Esto se debe a que la función debe recorrer la lista enlazada para encontrar el elemento a eliminar.
// Proposito: Quita un elemento dado.

    if (isEmptyS(s)) {
        return; // No se puede eliminar de un conjunto vacío
    }

    NodoS* nodoActual = s->primero;
    NodoS* nodoAnterior = NULL;

    // Buscar el nodo que contiene el elemento x
    while (nodoActual != NULL && nodoActual->elem != x) {
        nodoAnterior = nodoActual;
        nodoActual = nodoActual->siguiente;
    }

    // Si nodoActual es nullptr, significa que no se encontró el elemento x en el conjunto
    if (nodoActual == NULL) {
        return; // No se encontró el elemento, no se puede eliminar
    }

    // Caso especial: nodo a eliminar es el primero
    if (nodoAnterior == NULL) {
        s->primero = nodoActual->siguiente;
    } else {
        nodoAnterior->siguiente = nodoActual->siguiente;
    }

    // Liberar memoria del nodo a eliminar
    delete nodoActual;
    s->cantidad--;
}
int sizeS(Set s) {
// Proposito:Devuelve la cantidad de elementos.
    return (s->cantidad);
}
LinkedList setToList(Set s) {
// Proposito:Devuelve una lista con los lementos del conjunto.
    LinkedList lista = nil();

    NodoL* nodoActual = s->primero;
    if (s->cantidad > 0) {
        while(nodoActual->siguiente != NULL) {
        Snoc(nodoActual->elem, lista);
        nodoActual = nodoActual->siguiente
        }
        Snoc(nodoActual -> elem, lista);
    }

    return (lista);
}
void DestroyS(Set s) {
// Proposito:Libera la memoria ocupada por el conjunto
    NodoL* nodoActual = s->primero;
    NodoL* nodoSiguiente = NULL;
    while (nodoActual != NULL) {
        nodoSiguiente = nodoActual->siguiente;
        delete nodoActual;
    }
    delete s;
}