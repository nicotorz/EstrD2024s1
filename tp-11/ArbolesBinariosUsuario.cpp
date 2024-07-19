#include "ArbolesBinarios.h"

int sumarT(Tree t) {
// Dado un árbol binario de enteros devuelve la suma entre sus elementos.
    if (isEmptyT (t)) {
        return 0;
    } else {
        return rootT (t) + sumarT (left (t)) + sumarT (right (t));
    }
}

int sizeT(Tree t) {
// Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
    if (isEmptyT (t)) {
        return 0;
    } else {
        return 1 + sizeT (left t) + sizeT (right t);
    }
}

bool perteneceT(int e, Tree t) {
// Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
    if (isEmptyT (t)) {
        return false;
    } else {
        return t->elem == e || perteneceT (e, left (t)) || perteneceT (e, right(t));
    }
}

int aparicionesT(int e, Tree t) {
// Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
    if (isEmptyT (t)) {
        return 0;
    } else {
        int count = (t->elem == e) ? 1 : 0;
        return count + aparicionesT(e, left(t)) + aparicionesT(e, right(t));
    }
}

int heightT(Tree t) {
// Dado un árbol devuelve su altura.
    if (isEmptyT(t)) {
        return 0;
    }  else {
        return (1 + max(heightT(t->left), heightT(t->right)));
    }
}
ArrayList toList(Tree t) {
// Dado un árbol devuelve una lista con todos sus elementos.
    ArrayList lista = newArrayList(); Tree actualT = t;
    TList faltaProcesar = emptyTL();
    if (!isEmptyT(t)) {
        SnocTL(t, faltaProcesar);
    }
    while(!isEmptyTList(faltaProcesar)) {
        actualT = headTL(faltaProcesar);
        TailTl(faltaProcesar);
        lista = add(lista, actualT->elem);
        if (!isEmptyT(actual->right)) {
            SnocTL(actual->right, faltaProcesar);
        }
        if (!isEmptyT(actual->left)) {
            SnocTL(actual->left, faltaProcesar);
        }
    }
    LiberarTL(faltanProcesar);
    return (lista);

}

ArrayList leaves(Tree t) {
// Dado un árbol devuelve los elementos que se encuentran en sus hojas.
    ArrayList lista = newArrayList();
    if (isEmptyT(t)) {
        return lista;
    }
    if (isEmptyT(left(t)) && isEmptyT(right(t))) {
        // Es un nodo hoja
        lista = add(lista, rootT(t));
    } else {
        // Recorrer a los subarboles izquierdo y derecho
        ArrayList leftLeaves = leaves(left(t));
        ArrayList rightLeaves = leaves(right(t));
        lista = concat(lista, leftLeaves);
        lista = concat(lista, rightLeaves);
    }
    return lista;
}

void levelNDe(int n, Tree t, ArrayList xs){     // una n StackFrames, donde n es la cantidad de nodos del arbol, y tiene costo o(n) en la memoria heap, por el uso de los punteros al arbol
    if (n == 1){
        if (!isEmptyT(t)){
           add(rootT(t),xs);
        }
    }
    else{
        levelNDe((n-1),left(t),xs);
        levelNDe((n-1),right(t),xs);
    }
}

ArrayList levelN(int n, Tree t){            // se toma como nivel 1 la razi del arbol
    ArrayList xs = newArrayList();
    levelNDe(n,t,xs);
    return xs;
}