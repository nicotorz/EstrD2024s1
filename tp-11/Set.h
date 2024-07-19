#include <iostream>

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
Set emptyS();
// Proposito:Crea un conjunto vacío.
bool isEmptyS(Set s) ;
// Proposito:Indica si el conjunto está vacío.
bool belongsS(int x, Set s);
// Proposito:Indica si el elemento pertenece al conjunto.
void AddS(int x, Set s);
// Proposito:Agrega un elemento al conjunto.
void RemoveS(int x, Set s);
// Proposito: Quita un elemento dado.
int sizeS(Set s);
// Proposito:Devuelve la cantidad de elementos.
LinkedList setToList(Set s);
// Proposito:Devuelve una lista con los lementos del conjunto.
void DestroyS(Set s);
// Proposito:Libera la memoria ocupada por el conjunto