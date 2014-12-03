/*

(C) 2014 Lambda calculus with eager reduction semantics

*/


% Syntax of the lambda calculus
expr(var(X)) :- atom(X).
expr(apply(M, N)) :- expr(M), expr(N).
expr(lambda(X, M)) :- atom(X), expr(M).


% Normal form
normal(lambda(_, _)).


% Compute free variables of a term
fvars(var(X), [X]).
fvars(apply(M, N), V0) :-
    fvars(M, V1),
    fvars(N, V2),
    union(V1, V2, V0).
fvars(lambda(X, M), V0) :-
    fvars(M, V1),
    subtract(V1, [X], V0).


% Test for a closed term
closed(M) :- fvars(M, []).


%
% Eager evaluation
%
eval(apply(M1, N), apply(M2, N)) :-
    eval(M1, M2).
eval(apply(V, N1), apply(V, N2)) :-
    normal(V),
    eval(N1, N2).
eval(apply(lambda(X, M), V), N) :-
    normal(V),
    subst(X, V, M, N).

%
% Substitution
% subst(X, N, M1, M2): 
%    Substitute X by N in M1 returning M2
%
subst(X, N, var(X), N).
subst(X, _, var(Y), var(Y)) :- \+ X == Y.
subst(X, N, apply(M1, M2), apply(M3, M4)) :-
    subst(X, N, M1, M3),
    subst(X, N, M2, M4).
subst(X, _, lambda(X, M), lambda(X, M)).
subst(X, N, lambda(Y, M1), lambda(Y, M2)) :-
    fvars(N, FV),
    \+ member(Y, FV),
    subst(X, N, M1, M2).


/*

% Demo substitution

?- subst(x, var(z), var(x), M).
M = var(z) .

?- subst(x, var(z), var(y), M).
M = var(y) .

?- subst(x, var(z), lambda(y,apply(var(y),var(x))), M).
M = lambda(y, apply(var(y), var(z))) .

?- subst(x, var(z), lambda(z,apply(var(x),var(z))), M).
false.

*/
