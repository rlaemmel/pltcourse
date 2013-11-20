/*

The semantics of the simple expression language as of "introduction.pro" was given in the style of big-step operational semantics. Here, we provide a small-step operational semantics.

*/

% Import all the definitions for the language
:- ['introduction.pro'].

% Syntax of normal forms (aka values)
nf(const(_)).

% A step for the first operand of a binary expression
step(binary(E1, E2, O), binary(E3, E2, O)) :-
  step(E1, E3).

% A step for the second operand of a binary expression
step(binary(E1, E2, O), binary(E1, E3, O)) :-
  nf(E1),
  step(E2, E3).

% Application of multiplication
step(binary(const(C1), const(C2), mult), const(C0)) :-
  C0 is C1 * C2.

% Application of equality
step(binary(const(C1), const(C2), eq), const(B)) :-
  eqfun(C1, C2, B).

% A step for the condition part of the if-then-else
step(if(E1, E2, E3), if(E4, E2, E3)) :-
  step(E1, E4).

% Select then branch
step(if(const(true), E2, _), E2).

% Select else branch
step(if(const(false), _, E3), E3).

% Many-step reduction; reflexive case
reduce(X, X) :-
  nf(X).

% Many-step reduction; transitive case
reduce(X, Z) :-
  step(X, Y),
  reduce(Y, Z).

/*

?- wellTypedSample(X), step(X, Y).
X = if(binary(const(0), const(0), eq), const(42), binary(const(2), const(3), mult)),
Y = if(const(true), const(42), binary(const(2), const(3), mult)) .

?- wellTypedSample(X), reduce(X, Y).
X = if(binary(const(0), const(0), eq), const(42), binary(const(2), const(3), mult)),
Y = const(42) .

*/
