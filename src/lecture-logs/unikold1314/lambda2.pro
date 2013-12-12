/*

This is just an encoding of the basic typing rules for the lambda calculus, as discussed in the lecture.
Exercise: run / test these rules with some actual lambda expressions that are also evaluated.

*/

typeOf(G, var(X), T) :-
  member((X, T), G).

typeOf(G, apply(U, V), T2) :-
  typeOf(G, U, arrow(T1, T2)),
  typeOf(G, V, T1).

typeOf(G1, lambda(X, T1, U), arrow(T1, T2)) :-
  update(G1, X, T1, G2),
  typeOf(G2, U, T2).
