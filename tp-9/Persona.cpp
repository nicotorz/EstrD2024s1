using namespace std;
#include "Persona.h";

Persona nacer(String n) {
    Persona p;
    p.nombre = n; p.edad = 0;
    return (p);
}

Persona cumplirAnios(Persona p) {
    p.edad++;
    return(p);
}
string nombre(Persona p) {
    return(p.nombre);
}
int edad(Persona p) {
    return(p.edad);
}
void ShowPersona(Persona p) {
    cout << p << endl;
}