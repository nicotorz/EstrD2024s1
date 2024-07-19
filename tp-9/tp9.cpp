#include <iostream>
#include <string>

using namespace std;
// Propósito: imprime n veces un string s.
void printN(int n, string s) {
    for (int i = 0; i < n; i++)
    {
       cout<< s <<endl;
    }
    
}

void printNR(int n, string s) {
    if (n <= 0) {
        break;
    }
    cout<< s <<endl;
    printNR(n-1, s);
}

// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresiva(int n) {
    for (int i = n; i >= 0; i--)
    {
        cout<< i <<endl;
    }
}

void cuentaRegresivaR(int n) {
   if (n<0) {
    break;
   } 
   cout << n << endl;
   cuentaRegresivaR(n-1);
}
// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void desdeCeroHastaNR(int n) {

}
// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
int mult(int n, int m) {
    int resultado = 0;
    for (int i = 0; i < m; i++) 
    {
        resultado += n;
    }
    return resultado;
}

int multR(int n, int m) {
    if (m == 0 || n == 0) {
        return 0;
    }
    return n + mult(n, m-1); 
}

// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.
void primerosN(int n, string s) {
    for (int i = 0; i < n; i++)
    {
        cout << s[i] << endl;
    }
    
}

void primerosNR(int n, string s) {
    if (n<0) {
        break;
    }
    count << s[s.length - n] << endl;
    primerosNR (n - 1, s);
}

// Propósito: indica si un char c aparece en el string s.
bool pertenece(char c, string s) {
    for (int i = 0; i < s.lenght; i++)
    {
        if (c == s[i]) {
            return true;
        }
    }
    return false;
}

bool perteneceR(char c, string s) {
    if (s.length < 1) {
        return false;
    }
    if (s[0] == c) {
        return true;
    }
    return perteneceR(c, s.substr(1));
}

//Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int apariciones(char c, string s) {
    int apariciones = 0;
    for (int i = 0; i < s.lenght; i++)
    {
        if (c == s[i]) {
           apariciones++;
        }
    }
    return apariciones;
}

int aparicionesR(char c, string s) {
    if (s.length < 1) {
        return 0;
    }
    if (s[0] == c) {
        return 1;
    }
    return aparicionesR(c, s.substr(1));
}