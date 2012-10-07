/*

In (SWI-)Prolog, there are predicates maplist/2+ just like that.

*/

map(_,[]).
map(P,[H|T]) :- apply(P,[H]), map(P,T).

map(_,[],[]).
map(P,[H1|T1],[H2|T2]) :- apply(P,[H1,H2]), map(P,T1,T2).

increment(N1,N2) :- number(N1), N2 is N1 + 1.

/*

?- map(increment,[1,2,3],R).
R = [2, 3, 4]

*/
