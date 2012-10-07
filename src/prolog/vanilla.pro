/*

(C) 2011 Ralf Laemmel

This is folklore, but the basic code for solve/1 is from here:
http://kti.mff.cuni.cz/~bartak/prolog/meta_interpret.html
(C) 1998 Roman Barták
There is an extra clause for built-in predicates.

*/

:- ['eval.pro'].

solve(true).

solve((A,B)) :-
   solve(A),
   solve(B).

solve(A) :-
   predicate_property(A,built_in),
   !,
   call(A).

solve(A) :-
   clause(A,B),
   solve(B).


increment(X,Y) :- Y is X + 1.

% myClause(increment(X,Y),(Y is X + 1)).

%
% Use vanilla meta-interpreter
%

test3 :-
  see('eval.sample'),
  read(E),
  seen,
  % eval(E,V),
  solve(eval(E,V)),
  write(V), 
  nl.
