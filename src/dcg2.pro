/*

(C) 2011 Ralf Laemmel

A DCG for a simple imperative language with AST construction.
An existing interpreter is invoked.

*/

:- ['assign.pro'].

program([H|T]) --> expr(H), [;], rest(T).

rest([]) --> [].
rest(L)  --> program(L).

expr(E)   --> [num(N)], add(N,E).
expr(E)   --> [id(X)], add(X,E).
expr(X=E) --> [id(X)], [=], expr(E).

add(E,E) --> [].
add(E1,E1+E2) --> [+], expr(E2).

test :-
   program(L,[id(x),=,num(1),;,id(y),=,id(x),+,num(41),;],[]),
   write(L),
   nl,
   eval(L,R),
   write(R),
   nl.
