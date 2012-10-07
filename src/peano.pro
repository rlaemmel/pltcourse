/* 

(C) 2011 Ralf Laemmel

Peano numbers

*/


% Natural numbers

nat(zero).
nat(succ(X)) :- nat(X).


% Even natural numbers

even(zero).
even(succ(succ(X))) :- even(X).


% Addition

add(zero,X,X).
add(succ(X),Y,succ(Z)) :- add(X,Y,Z).


% Conversion to integer

toInteger(zero,0).
toInteger(succ(X),Y) :- 
  toInteger(X,Z), Y is Z + 1.


% Conversion from integer

fromInteger(X,Y) :-
  integer(X),
  (X == 0 ->
     Y = zero;
     X > 0, 
     Z is X - 1,
     fromInteger(Z,V),
     Y = succ(V)).


/*

?- add(succ(succ(zero)),succ(zero),X).
X = succ(succ(succ(zero))).

?- toInteger(succ(succ(zero)),X).
X = 2.

?- fromInteger(3,X).
X = succ(succ(succ(zero))) ;
false.



*/
