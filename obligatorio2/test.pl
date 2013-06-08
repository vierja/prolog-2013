get_val_matriz(X, Y) :-
    Max is 10,
    writeln('get_val_matriz -  Max:' + Max),
    between(0, Max, X),
    between(0, Max, Y),
    writeln('Posicion - X, Y' + X + ', ' + Y + ' tiene valor ' + Turno).


get_val_matriz(X, Y).
