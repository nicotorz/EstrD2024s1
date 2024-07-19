#include <iostream>

int main() {
    count << 1;
}

struct Par {
    int x;
    int y;
};

Par consPar(int x, int y);
// Propósito: construye un par
    struct Par p;
    p.x = x;
    p.y = y;
    return p;

int fst(Par p);
// Propósito: devuelve la primera componente
    return p.x;

int snd(Par p);
// Propósito: devuelve la segunda componente
    return p.y;

int maxDelPar(Par p);
// Propósito: devuelve la mayor componente
    if (p.x > p.y) {
        return p.x;
    } else {
        return p.y;
    }

Par swap(Par p);
// Propósito: devuelve un par con las componentes intercambiadas
    return consPar(snd(p), fst(p));

Par divisionYResto(int n, int m);
// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
    int division = n / m;
    int resto    = n % m;
    return consPar(division, resto);

void printN(int n, string s) {
// Propósito: imprime n veces un string s.
    for (int i=n; i!=0; i--) {
        cout << s << endl;
    }
}
void cuentaRegresiva(int n) {
// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
    for (int i=n; i>=0; i--) {
        cout << i << endl;
    }
}
void desdeCeroHastaN(int n) {
// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
    int contador = 0;
    for (int i=n; i>=0; i--) {
        cout << contador << endl;
        contador++;
    }
}
int mult(int n, int m) {
// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
    int resultado = 0;
    for (int i=0; i < m; i++) {
       resultado += n;
    }
    return resultado;
}
void primerosN(int n, string s) {
// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.
    for (int i=0; i<n; i++) {
        cout << s[i] << endl;
    }
}
bool pertenece(char c, string s) {
// Propósito: indica si un char c aparece en el string s.
    for (int i=0; i < s.length(); i++) {
        if (c == s[i]) {
            return true;
        }
    }
    return false;
}

int apariciones(char c, string s){
//Propósito: devuelve la cantidad de apariciones de un char c en el string s.
    int apariciones = 0;
    for (int i=0; i < s.length(); i++) {
        if (c == s[i]) {
            apariciones += 1;
        }
    }
    return apariciones;
}