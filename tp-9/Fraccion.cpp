using namespace std;
#include "Fraccion.h"

Fraccion consFraccion(int numerador, int denominador) {
// Propósito: construye una fraccion
// Precondición: el denominador no es cero
    Fraccion f;
    f.numerador = numerador; f.denominador = denominador;
    return (f);
}
int numerador(Fraccion f) {
// Propósito: devuelve el numerador
    return (f.numerador);
}
int denominador(Fraccion f) {
// Propósito: devuelve el denominador
    return (f.denominador);
}
float division(Fraccion f) {
// Propósito: devuelve el resultado de hacer la división
    return (f.numerador / f.denominador);
}
Fraccion multF(Fraccion f1, Fraccion f2) {
// Propósito: devuelve una fracción que resulta de multiplicar las fracciones (sin simplificar)
    Fraccion resultado;
    resultado.numerador = f1.numerador * f2.numerador;
    resultado.denominador = f1.denominador * f2.denominador;
    return resultado;
}
Fraccion simplificada(Fraccion p) {
// Propósito: devuelve una fracción que resulta de simplificar la dada por parámetro
    int divisorComun = mcd(p.numerador, p.denominador);
    Fraccion resultado;
    resultado.numerador = p.numerador / divisorComun;
    resultado.denominador = p.denominador / divisorComun;
    return resultado;
}
Fraccion sumF(Fraccion f1, Fraccion f2) {
// Propósito: devuelve la fracción resultante de sumar las fracciones
    Fraccion resultado;
    resultado.numerador = f1.numerador * f2.denominador + f2.numerador * f1.denominador;
    resultado.denominador = f1.denominador * f2.denominador;
    return resultado;
}