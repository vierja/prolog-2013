/* 
    nth(?L,+Pos,?E)
    E es el elemento en la posicion Pos (contando a partir de 1) de la lista L.
    Ej. nth(2, [a,b,c], b)
*/

nth(L, Pos, E) :- Z is Pos-1, nth0(Z, L, E).


