/*

(C) 2011 Ralf Laemmel

Some experiments with a directed graph.

*/

node(1).
node(2).
node(3).

edge(1,2).
edge(2,3).

connected(X,Y) :-
  edge(X,Y).

connected(X,Y) :-
  edge(X,Z),
  connected(Z,Y).


