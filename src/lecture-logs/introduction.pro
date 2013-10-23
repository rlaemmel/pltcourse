/*

Sample program in a simple expression-oriented language:

if 0=0 then true else 2*3

Let's defined syntax, type system, and semantics for that language.

Syntax in BNF:

expr = const 
     | expr op expr 
     | "if" expr "then" expr "else" expr
op = "*" | "="
const = "true" | "false" | int

Let's switch to Prolog.

*/

% Syntax of expressions
expr(const(C)) :- const(C).
expr(binary(E1, E2, O)) :-
  expr(E1), 
  expr(E2),
  op(O).
expr(if(E1, E2, E3)) :-
  expr(E1), 
  expr(E2),
  expr(E3).

% Binary operators
op(mult).
op(eq).

% Constants
const(true).
const(false).
const(I) :- integer(I).

% A well-typed sample
wellTypedSample(
  if(
     binary(const(0), const(0), eq),
     const(42),
     binary(const(2), const(3), mult)
    )
).

An ill-typed sample
illTypedSample(
  if(
     binary(const(0), const(0), eq),
     const(true),
     binary(const(2), const(3), mult)
    )
).

% Syntax of types
type(bool).
type(int).

% Predicate for well-typedness
wellTyped(const(true), bool).
wellTyped(const(false), bool).
wellTyped(const(I), int) :- integer(I).
wellTyped(binary(E1, E2, mult), int) :-
  wellTyped(E1, int),
  wellTyped(E2, int).
wellTyped(binary(E1, E2, eq), bool) :-
  wellTyped(E1, T),
  wellTyped(E2, T).
wellTyped(if(E1, E2, E3), T) :-
  wellTyped(E1, bool),
  wellTyped(E2, T),
  wellTyped(E3, T).

/*

For example:

?- wellTypedSample(E), wellTyped(E, T).
E = if(binary(const(0), const(0), eq), const(42), binary(const(2), const(3), mult)),
T = int .

*/

% Syntax of values
value(V) :- const(V).

/*
% Alternative syntax of values
value(ival(I)) :- integer(I).
value(bval(B)) :- boolean(B).
boolean(true).
boolean(false).
*/

% Big-step semantics of expressions
eval(const(C), C).
eval(binary(E1, E2, mult), C0) :-
  eval(E1, C1), % integer(C1),
  eval(E2, C2), % integer(C2),
  C0 is C1 * C2.
eval(binary(E1, E2, eq), B) :-
  eval(E1, C1),
  eval(E2, C2),
  eqfun(C1, C2, B).
eval(if(E1, E2, _), V2) :-
  eval(E1, true),
  eval(E2, V2).
eval(if(E1, _, E3), V3) :-
  eval(E1, false),
  eval(E3, V3).

% A helper predicate for equality
eqfun(C1, C2, true) :- C1 == C2.
eqfun(C1, C2, false) :- C1 \= C2.

/*

For example:

?- wellTypedSample(E), eval(E, V).
E = if(binary(const(0), const(0), eq), const(42), binary(const(2), const(3), mult)),
V = 42 .

*/

% Combine well-typedness and evaluation
wellTypedEval(E, V) :-
  wellTyped(E, _),
  eval(E, V).
