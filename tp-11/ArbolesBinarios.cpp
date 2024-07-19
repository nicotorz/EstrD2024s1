struct NodeT {
int elem;
NodeT* left;
NodeT* right;
}
typedef NodeT* Tree;

Tree emptyT() {
    return (NULL);
}

Tree nodeT(int elem, Tree left, Tree right) {
    NodeT* t = new NodeT;
    t->value = elem;
    t->left  = left;
    t->right = right;
    return (t);
}

bool isEmptyT(Tree t) {
    return (t==NULL);
}

int rootT(Tree t) {
    if (isEmptyT(t)) {
        throw std::runtime_error("El arbol esta vacio");
    }
    return t->elem;
}

Tree left(Tree t) {
    if (isEmptyT(t)) {
        throw std::runtime_error("El arbol esta vacio");
    }
    return (t->right);
}

Tree right(Tree t) {
    if (isEmptyT(t)) {
        throw std::runtime_error("El arbol esta vacio");
    }
    return (t->left);
}

void DeleteT (Tree t) {
    if (!isEmptyT (t)) {
        DeleteT (t->left);
        DeleteT (t->right);
        delete t;
    }
}