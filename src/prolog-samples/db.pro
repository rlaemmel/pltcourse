/* 

(C) 2011 Ralf Laemmel

Demonstration of a Prolog DB example.
Terms (lists of pairs) are turned into facts.

*/

assertEdge((X,Y)) :-
  assertz(edge(X,Y)).

connected(X,Y) :-
  edge(X,Y).

connected(X,Y) :-
  edge(X,Z),
  connected(Z,Y).

% :- dynamic edge/2.

test :-
  L = [(1,2),(2,3)],
%  \+ connected(1,3),
  maplist(assertEdge,L),
  connected(1,3).
