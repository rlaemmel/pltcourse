/*

(C) 2011 Ralf Laemmel

This is a vanilla metainterpreter with proof-tree synthesis.
The basic code for solve/2 is from here:
http://kti.mff.cuni.cz/~bartak/prolog/meta_interpret.html
(C) 1998 Roman Barták
There is an extra clause for built-in predicates.

*/

:- ['eval.pro'].

solve(true,fact).

solve((A,B),(ProofA,ProofB)) :-
   solve(A,ProofA),
   solve(B,ProofB).

solve(A,A-built_in) :-
   predicate_property(A,built_in),
   !,
   call(A).

solve(A,A-ProofB) :-
   clause(A,B),
   solve(B,ProofB).


%
% Use the meta-interpreter
%

test3 :-
  see('eval.sample'),
  read(E),
  seen,
  solve(eval(E,V),P),
  write(V), 
  nl,
  write(P), 
  nl.
