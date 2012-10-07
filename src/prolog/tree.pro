/*

(C) 2011 Ralf Laemmel

Some experiments with binary trees.

*/

tree(leaf(X)) :- integer(X).
tree(fork(T1,T2)) :- tree(T1), tree(T2).

max(leaf(X),X).
max(fork(T1,T2),X) :- max(T1,Y), max(T2,Z), X is max(Y,Z).

/*

?- max(fork(leaf(1),fork(leaf(42),leaf(88))),X).
X = 88.

*/
