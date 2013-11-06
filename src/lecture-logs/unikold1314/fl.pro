/*

Let's define the "factorial language" (FL).
It is powerful enough to define the factorial function.

*/

:- ['prelude.pro'].

% Syntax of programs (lists of fun defs and main expression)
prog((Defs, E)) :-
  map(def, Defs),
  expr(E).

% Syntax of function definitions
def((FN, ANs, E)) :-
  atom(FN),
  map(atom, ANs),
  expr(E).

% Syntax of constants
expr(const(C)) :- const(C).

% Syntax of binary expressions
expr(binary(E1, E2, O)) :-
  expr(E1), 
  expr(E2),
  op(O).

% Syntax of conditionals
expr(if(E1, E2, E3)) :-
  expr(E1), 
  expr(E2),
  expr(E3).

% Syntax of function application
expr(apply(FN, Es)) :-
  atom(FN), 
  map(expr, Es).

% Syntax of function argument references
expr(var(N)) :-
  atom(N).

% Binary operators
op(mult).
op(sub).
op(eq).

% Constants
const(true).
const(false).
const(I) :- integer(I).

% The factorial function
sample(
  (
    [
      ( 
        factorial,
        [x], 
        if(
          binary(var(x), const(0), eq),
          const(1),
          binary(
            var(x),
            apply(
              factorial,
              [binary(var(x),const(1),sub)]),
            mult)
        )
      )
    ],
    apply(factorial, [const(5)])
  )
).

% Semantics of constants
eval(_, _, const(C), C).

% Semantics of multiplication
eval(Defs, Env, binary(E1, E2, mult), C0) :-
  eval(Defs, Env, E1, C1), % integer(C1),
  eval(Defs, Env, E2, C2), % integer(C2),
  C0 is C1 * C2.

% Semantics of subtraction
eval(Defs, Env, binary(E1, E2, sub), C0) :-
  eval(Defs, Env, E1, C1), % integer(C1),
  eval(Defs, Env, E2, C2), % integer(C2),
  C0 is C1 - C2.

% Semantics of equality
eval(Defs, Env, binary(E1, E2, eq), B) :-
  eval(Defs, Env, E1, C1),
  eval(Defs, Env, E2, C2),
  eqfun(C1, C2, B).

% Semantics of conditional with true condition
eval(Defs, Env, if(E1, E2, _), V2) :-
  eval(Defs, Env, E1, true),
  eval(Defs, Env, E2, V2).

% Semantics of conditional with false condition
eval(Defs, Env, if(E1, _, E3), V3) :-
  eval(Defs, Env, E1, false),
  eval(Defs, Env, E3, V3).

% Semantics of argument access
eval(_, Env, var(N), V) :-
  member((N, V), Env).

% Semantics of function application
eval(Defs, Env, apply(FN, Es), V)
 :-

    % Evaluate all actual arguments
    map(eval(Defs, Env), Es, Vs),

    % Look up function from list of definitions
    member((FN, ANs, E), Defs),

    % Build local environment
    zip(ANs, Vs, LocalEnv),

    % Evaluate function body
    eval(Defs, LocalEnv, E, V).

% A helper predicate for equality
eqfun(C1, C2, true) :- C1 == C2.
eqfun(C1, C2, false) :- C1 \= C2.

% Semantics of programs
eval((Defs, E), V) :-
  eval(Defs, [], E, V).
