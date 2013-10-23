/*

(C) 2011 Ralf Laemmel

Different approaches for dealing with multiple-solution predicates.

*/

filter(_,[],[]).
filter(P,[H|T],R) :-
  ( apply(P,[H]) -> R = [H|RR]; R = RR),
  filter(P,T,RR).

greaterThan42(X) :- X > 42.

/*

% "Manual" findall

?- member(X,[40,41,42,43,44]), X > 42.
X = 43 ;
X = 44.

% Use filter/3 predicate

?- filter(greaterThan42,[40,41,42,43,44],R).
R = [43, 44] .

% Use findall/3, finally.

?- findall(
  X,
  ( 
    member(X,[40,41,42,43,44]),
    X > 42
  ),
  L).
L = [43, 44].

*/
