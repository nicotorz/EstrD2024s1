struct NodoL {
    int elem; // valor del nodo
    NodoL* siguiente; // puntero al siguiente nodo
}
struct LinkedListSt {
// INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
// desde primero por siguiente hasta alcanzar a NULL
    int cantidad; // cantidad de elementos
    NodoL* primero; // puntero al primer nodo
}
typedef LinkedListSt* LinkedList; // INV.REP.: el puntero NO es NULL
    struct IteratorSt {
    NodoL* current;
}
typedef IteratorSt* ListIterator; // INV.REP.: el puntero NO es NULL
LinkedList nil();
//Proposito: Crea una lista vacía.
bool isEmpty(LinkedList xs);
//Proposito: Indica si la lista está vacía.
int head(LinkedList xs);
//Proposito: Devuelve el primer elemento.
void Cons(int x, LinkedList xs);
//Proposito: Agrega un elemento al principio de la lista.
void Tail(LinkedList xs);
//Proposito: Quita el primer elemento.
int length(LinkedList xs);
//Proposito: Devuelve la cantidad de elementos.
void Snoc(int x, LinkedList xs);
//Proposito: Agrega un elemento al final de la lista.
ListIterator getIterator(LinkedList xs);
//Proposito: Apunta el recorrido al primer elemento.
int current(ListIterator ixs);
//Proposito: Devuelve el elemento actual en el recorrido.
void SetCurrent(int x, ListIterator ixs);
//Proposito: Reemplaza el elemento actual por otro elemento.
void Next(ListIterator ixs);
//Proposito: Pasa al siguiente elemento.
bool atEnd(ListIterator ixs);
//Proposito: Indica si el recorrido ha terminado.
void DisposeIterator(ListIterator ixs);
//Proposito: Libera la memoria ocupada por el iterador.
void DestroyL(LinkedList xs);
//Proposito: Libera la memoria ocupada por la lista.