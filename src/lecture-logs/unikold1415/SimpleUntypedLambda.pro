/*

(C) 2014 The simple untyped lambda calculus with eager reduction semantics

*/


%
% Syntax of the lambda calculus
%
expr(var(X)) :- atom(X).
expr(apply(M, N)) :- expr(M), expr(N).
expr(lambda(X, M)) :- atom(X), expr(M).


%
% Normal form
%
normal(lambda(_, _)).


%
% Generate a new variable
% This is a non-declarative solution!
% Exercise: make it more declarative.
%
nvar(V, X) :-
    gensym('_', X), % non-declarative!
    ( member(X, V) ->
	  nvar(V, X); true ).


%
% Compute free variables of a term
%
fvars(var(X), [X]).
fvars(apply(M, N), V0) :-
    fvars(M, V1),
    fvars(N, V2),
    union(V1, V2, V0).
fvars(lambda(X, M), V0) :-
    fvars(M, V1),
    subtract(V1, [X], V0).


%
% Test for a closed term
%
closed(M) :- fvars(M, []).


%
% Eager evaluation -- small-step semantics
%

step(apply(M1, N), apply(M2, N)) :-
    step(M1, M2).
step(apply(V, N1), apply(V, N2)) :-
    normal(V),
    step(N1, N2).
step(apply(lambda(X, M), V), N) :-
    normal(V),
    subst(X, V, M, N).

reduce(M1, M3) :-
    step(M1, M2) ->
	reduce(M2, M3); M3 = M1.


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
    \+ X == Y,
    fvars(N, FV),
    \+ member(Y, FV),
    subst(X, N, M1, M2).
subst(X, N, lambda(Y, M1), lambda(Z, M3)) :-
    \+ X == Y,
    fvars(N, FV),
    member(Y, FV),
    nvar(FV, Z),
    subst(Y, var(Z), M1, M2),
    subst(X, N, M2, M3).

/*

% Demo substitution

?- subst(x, var(z), var(x), M).
M = var(z) .

?- subst(x, var(z), var(y), M).
M = var(y) .

?- subst(x, var(z), lambda(y,apply(var(y),var(x))), M).
M = lambda(y, apply(var(y), var(z))) .

?- subst(x, var(z), lambda(z,apply(var(x),var(z))), M).
M = lambda('_1', apply(var(z), var('_1'))) .

*/


%
% A sample term
%   In Haskell syntax: if True then 0 else 1
%
sample(
	apply(
		apply(
			apply(
				% if-then-else
				lambda(l,lambda(m,lambda(n,apply(apply(var(l),var(m)),var(n))))),
				% True
				lambda(t,lambda(f,var(t)))),
			% Church numeral 0
			lambda(s, lambda(z, var(z)))),
		% Church numeral 1
		lambda(s, lambda(z, apply(var(s), apply(var(s), var(z))))))).


/*

% Demo reduction

sample(M1), reduce(M1, M2).
M1 = apply(apply(apply(lambda(l, lambda(m, lambda(n, apply(apply(var(l), var(m)), var(n))))), lambda(t, lambda(f, var(t)))), lambda(s, lambda(z, var(z)))), lambda(s, lambda(z, apply(var(s), apply(var(s), var(z)))))),
M2 = lambda(s, lambda(z, var(z))).

% Thus, the result is 0, as expected.

*/
