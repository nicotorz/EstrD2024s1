#include "Persona.h"
using namespace std;

struct PersonaSt {
    string nombre;
    int edad;
};

Persona consPersona(string n, int e) {
    PersonaSt* p = new PersonaSt;
    p->edad = e; p->nombre = n;
    return p;
}

string nombre(Persona p) {
    return (p->nombre);
}

int edad(Persona p) {
    return (p->edad);
}

void crecer(Persona p) {
    p->edad++;
}

void cambioNombre(string n, Persona p) {
    p->nombre = n;
}

bool esMayorQueLaOtra(Persona p1, Persona p2) {
    return (edad p1 > edad p2);
}

Persona laQueEsMayor(Persona p1, Persona p2) {
    if (esMayorQueLaOtra (p1, p2)) {
        return p1;
    } else { 
        return p2;
    }
}