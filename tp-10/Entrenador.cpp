#include "Entrenador.h"
#include "Pokemon.h"

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
//Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve un entrenador.
    EntrenadorSt* e = new EntrenadorSt;
    e->nombre = nombre;
    e->cantidad = cantidad;
    e->pokemon = new Pokemon[cantidad];
    for (int i = 0; i < cantidad; ++i) {
        e->pokemones[i] = pokemones[i];
    }
    return e;
}
string nombreDeEntrenador(Entrenador e) {
//Devuelve el nombre del entrenador.
    return (e->nombre);
}
int cantidadDePokemon(Entrenador e) {
//Devuelve la cantidad de pokémon que posee el entrenador.
    return (e->cantPokemon);
}
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
//Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
    int cantidadP = 0;
    for (int i = 0; i < cantidadDePokemon e; ++i) {
        if (tipoDePokemon (e->pokemon[i]) == tipo) {
            ++cantidadP;
        }
    }
    return cantidadP;
}
Pokemon pokemonNro(int i, Entrenador e) {
//Devuelve el pokémon número i de los pokémon del entrenador.
//Precondición: existen al menos i − 1 pokémon.
    return (e->pokemon[i]);
}
bool leGanaATodos(Entrenador e1, Entrenador e2) {
//Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero posee al menos un pokémon que le gane.
    for (int i = 0; i < e2->cantPokemon; ++i) {
        if (!algunoLeGana (e1, e2->pokemon[i])) {
            return false;
        }
    }
    return true;
}

bool algunoLeGana(Entrenador e1, Pokemon pokemon) {
    for (int i = 0; i < e1->cantPokemon (e1); ++i) {
        if (superaA ((e1->pokemon[i]), pokemon)) {
            return true;
        } 
    }
    return false;
}