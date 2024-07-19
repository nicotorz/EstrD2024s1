struct NodoQ {
int elem; // valor del nodo
NodoQ* siguiente; // puntero al siguiente nodo
}
struct QueueSt {
int cantidad; // cantidad de elementos
NodoQ* primero; // puntero al primer nodo
NodoQ* ultimo; // puntero al ultimo nodo
}
typedef QueueSt* Queue;
// Definir la siguiente interfaz de este tipo de colas, respetando el costo de las operaciones:
// INV.REP. : El primero es un puntero a NULL si solo si el ultimo es un puntero a NULL.
//            Cantidad no puede ser negativo, y solamente es 0 cuando no hay elementos.
Queue emptyQ() {
// Crea una cola vacía.
// Costo: O(1).
    QueueSt* q = new QueueSt;
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;
}
bool isEmptyQ(Queue q) {
// Indica si la cola está vacía.
// Costo: O(1).
    return (q->cantidad == 0);
}
int firstQ(Queue q) {
// Devuelve el primer elemento.
// Costo: O(1).
    return (q->primero);
}
void Enqueue(int x, Queue q) {
// Agrega un elemento al final de la cola.
// Costo: O(1).

    NodoQ* nuevoNodo = new NodoQ;
    nuevoNodo->elem = x;
    nuevoNodo->siguiente = NULL;
    if (q->cantidad == 0) {
        q->primero = nuevoNodo;
        q->ultimo  = nuevoNodo;
    } else {
        q->ultimo->siguiente = nuevoNodo;
        q->ultimo = nuevoNodo;
    }
    q->cantidad++;
}

void Dequeue(Queue q) {
// Quita el primer elemento de la cola.
// Costo: O(1).
    if (isEmptyQ (q)) {
        cout << "Error: no hay elementos para quitar" << endl;
    }
    NodoQ* nodoAQuitar = q->primero; 
    q->primero = nodoAQuitar->siguiente;
    delete nodoAQuitar;

    q->cantidad--;
}
int lengthQ(Queue q) {
// Devuelve la cantidad de elementos de la cola.
// Costo: O(1).
    return (q->cantidad);
}
void MergeQ(Queue q1, Queue q2) {
// Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
// Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
// Costo: O(1).
    if (isEmptyQ(q1)) {
        q1->primero = q2->primero;
        q1->ultimo = q2->ultimo;
    } else {
        q1->ultimo->siguiente = q2->primero;
        q1->ultimo = q2->ultimo;
    }
    q1->canitidad += q2->cantidad;
    
    delete q2;
}
void DestroyQ(Queue q) {
// Libera la memoria ocupada por la cola.
// Costo: O(n).
    NodoQ* nodoActual = q->primero; 
    while (nodoActual != NULL) {
        Dequeue(q);
        nodoActual = q->primero
    }
    delete q;
}
