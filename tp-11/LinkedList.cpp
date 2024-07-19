typedef LinkedListSt* LinkedList; // INV.REP.: el puntero NO es NULL

struct IteratorSt {
    NodoL* current;
}

struct LinkedListSt {
// INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
// desde primero por siguiente hasta alcanzar a NULL
    int cantidad; // cantidad de elementos
    NodoL* primero; // puntero al primer nodo
}

typedef IteratorSt* ListIterator; // INV.REP.: el puntero NO es NULL

LinkedList nil() {
// Proposito: Crea una lista vacía.
    LinkedList* xs = new LinkedList;
    xs->cantidad = 0; 
    xs->primero = NULL;
    return xs;
}

bool isEmpty(LinkedList xs) {
//Proposito: Indica si la lista está vacía.
    return (xs->cantidad == 0);
}

int head(LinkedList xs) {
//Proposito: Devuelve el primer elemento.
    if (isEmpty xs) {
        cout << "Error: la lista está vacía." << endl;
        exit(1);
    }
    return (xs->primero->elem);
}

void Cons(int x, LinkedList xs) {
//Proposito: Agrega un elemento al principio de la lista.
    NodoL* nuevoE = new NodoL;
    nuevoE->elem = x;
    nuevoE->siguiente = xs->primero;
    xs->primero = nuevoE;
    xs->cantidad++;
}
void Tail(LinkedList xs) {
//Proposito: Quita el primer elemento.
    NodoL* viejoE = xs->primero;
    xs->primero = viejoE->siguiente;
    xs->cantidad--;
    delete viejoE;
}
int length(LinkedList xs) {
//Proposito: Devuelve la cantidad de elementos.
    return (xs->cantidad);
}
void Snoc(int x, LinkedList xs) {
//Proposito: Agrega un elemento al final de la lista.
    NodoL* nuevoNodo = new NodoL;
    nuevoNodo->elem = x;
    nuevoNodo->siguiente = NULL;

    if (isEmpty(xs)) {
        xs->primero = nuevoNodo;
    } else {
        NodoL* nodoActual = xs->primero;
        while (nodoActual->siguiente != NULL) {
            nodoActual = nodoActual->siguiente;
        }
        nodoActual->siguiente = nuevoNodo;
    }
    xs->cantidad++;
}

void Append(LinkedList xs, LinkedList ys) {
// Agrega todos los elementos de la segunda lista al final de los de la primera.
// La segunda lista se destruye.
    NodoL* nodoActual = primero->ys;
    while (nodoActual != NULL) {
        Snoc (nodoActual->elem, xs);
        NodoL* nodoAEliminar = nodoActual;
        nodoActual = nodoActual->siguiente;
        delete nodoAEliminar; // limpia la memoria de cada nodo de ys a medida que se transfiere a xs.
    }
    delete ys; // destruye la estructura de la lista ys.
}

ListIterator getIterator(LinkedList xs) {
//Proposito: Apunta el recorrido al primer elemento.
    IteratorSt* iter = new IteratorSt;
    iter->current = xs->primero;
    return iter;
}

int current(ListIterator ixs) {
//Proposito: Devuelve el elemento actual en el recorrido.
     if (ixs->current == NULL) {
        cout << "Error: el iterador está fuera del rango de la lista." << endl;
        exit(1);
    }
    return (ixs->current->elem); 
}
void SetCurrent(int x, ListIterator ixs) {
//Proposito: Reemplaza el elemento actual por otro elemento.
    ixs->current->elem = x;
}

void Next(ListIterator ixs) {
//Proposito: Pasa al siguiente elemento.
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs) {
//Proposito: Indica si el recorrido ha terminado.
    return (ixs->current == NULL);
}


void DisposeIterator(ListIterator ixs) {
//Proposito: Libera la memoria ocupada por el iterador.
    delete ixs
}

void DestroyL(LinkedList xs) {
//Proposito: Libera la memoria ocupada por la lista.
    while (xs->primero != NULL) {
        NodoL* nodoActual = xs->primero;
        xs->primero = nodoActual->siguiente;
        delete nodoActual;
    }
    delete xs;
}

