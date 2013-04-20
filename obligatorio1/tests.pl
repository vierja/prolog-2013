:- begin_tests(lists).
:- use_module(library(lists)).

% nth

test(nth_1) :-
        nth([a,b,c], 2, b), !.

test(nth_2, [fail]) :-
        nth([a,b,c], 2, a), !.

test(nth_3) :-
        findall(X, nth([a,b,c], 2, X), Xs),
        Xs = [b], !.

test(nth_4, [fail]) :-
        findall(X, nth([a,b,c], 2, X), Xs),
        Xs = [a], !.

test(nth_5) :-
        nth(X, 2, b),
        nth1(2, X, b), !.

% sublist_n

test(sublist_n_1) :-
        sublist_n([a,b,c], 2, [a,b]), !.

test(sublist_n_2, [fail]) :-
        sublist_n([a,b,c], 3, [a,b]), !.

test(sublist_n_3, [fail]) :-
        sublist_n([a,b,c], 2, [a,c]), !.

test(sublist_n_4) :-
        sublist_n([a,b,c], 0, []), !.

test(sublist_n_5) :-
        findall(X,  sublist_n([a,b,c], 2, X), Xs),
        Xs = [[b,c], [a,b]], !.

% palindromo

test(palindromo0) :-
        palindromo([]), !.

test(palindromo1) :-
        palindromo([1]), !.

test(palindromo2) :-
        palindromo([1,2,1]), !.

test(palindromo3) :-
        palindromo([1,2,4,5,2,1,2,5,4,2,1]), !.

test(palindromo3) :-
        palindromo([1,2,4,5,2,2,5,4,2,1]), !.

test(palindromo4, [fail]) :-
        palindromo([1,2]), !.

test(palindromo5, [fail]) :-
        palindromo([1,2,3]), !.

% merge

test(merge0) :-
        merge([],[],[]), !.

test(merge1) :-
        merge([A],[],[A]), !.

test(merge2) :-
        merge([],[A],[A]), !.

test(merge3) :-
        merge([1,2,3],[4],[1,2,3,4]), !.

test(merge4) :-
        merge([1,2,3,4,5,6],[1,2,3,4,5],[1,1,2,2,3,3,4,4,5,5,6]), !.

test(merge5) :-
        merge([2,4,6],[1,3,5,7,9],[1,2,3,4,5,6,7,9]), !.

test(merge6) :-
        merge([1,3,5,7,9],[2,4,6],[1,2,3,4,5,6,7,9]), !.

test(merge7) :-
        merge([1],[1],[1,1]), !.

test(merge8, [fail]) :-
        merge([1],[2],[2,1]), !.

test(merge9) :-
        findall(X,  merge([1,6,7], [6,8,3], X), Xs),
        Xs = [[1,6,6,7,8,3]], !.

% member_sorted

test(member_sorted0) :-
        member_sorted([],[]), !.

test(member_sorted1) :-
        member_sorted([9,8,7,6,5,4,3,2,1],[1,2,3,4,5,6,7,8,9]), !.

test(member_sorted2) :-
        member_sorted([1],[1]), !.

test(member_sorted3) :-
        findall(X, member_sorted([6,42,3,5,1,99], X), Xs),
        Xs = [[1,3,5,6,42,99]].

% permutacion

test(permutacion0) :-
        permutacion([],[]), !.

test(permutacion1) :-
        permutacion([1,6,3,2,1,7,8],[7,1,3,1,6,8,2]), !.

test(permutacion3, [fail]) :-
        permutacion([1], [1,2]), !.

test(permutacion4) :-
        findall(X, permutacion([1,2], X), Xs),
        Xs = [[1,2], [2,1]].

test(permutacion5) :-
        findall(X, permutacion([1], X), Xs),
        Xs = [[1]].

test(permutacion6) :-
        findall(X, permutacion([1,2,3], X), Xs),
        Xs = [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]].

% remove_elem

test(remove_elem0) :-
        remove_elem([1,2],1,[2]), !.

test(remove_elem1) :-
        remove_elem([1,2],2,[1]), !.

:- end_tests(lists).