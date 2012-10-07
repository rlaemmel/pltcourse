/* 

(C) 2011 Ralf Laemmel

A simple expression evaluator

*/

eval(num(N),N) :-
  number(N).

eval(add(E1,E2),N) :-
  eval(E1,N1),
  eval(E2,N2),
  N is N1 + N2.

expr(num(N)) :- number(N).
expr(add(E1,E2)) :- expr(E1), expr(E2).

% File I/O Edinburgh style
test :-
  see('eval.sample'),
  read(E),
  seen,
  eval(E,V),
  write(V), 
  nl.

% File I/O ISO style

test2 :-
  open('eval.sample',read,In),
  read(In,E),
  close(In),
  eval(E,V),
  write(V), 
  nl.


/*

?- eval(add(add(num(1),num(2)),num(3)),X).
X = 6.

?- test.
6
true.

?- expr(add(num(1),num(2))).
true.

?- expr(foo).
false.

*/
