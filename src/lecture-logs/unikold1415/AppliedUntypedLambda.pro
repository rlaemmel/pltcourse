/*

(C) 2014 An applied untyped lambda calculus with eager evaluation

*/

:- multifile expr/1.
:- multifile normal/1.
:- multifile fvars/2.
:- multifile step/2.
:- multifile subst/4.
:- ['SimpleUntypedLambda.pro'].
:- abolish(sample/1).


%
% Syntax cont'd
%
expr(number(N)) :- number(N).
expr(add(E1, E2)) :- expr(E1), expr(E2).
expr(mul(E1, E2)) :- expr(E1), expr(E2).
expr(true).
expr(false).
expr(not(E)) :- expr(E).
expr(null(E)) :- expr(E).
expr(if(E1, E2, E3)) :- expr(E1), expr(E2), expr(E3).


%
% Normal forms cont'd
%
normal(true).
normal(false).
normal(number(_)).


%
% Free variables cont'd
%
fvars(number(_), []).
fvars(add(M, N), V0) :-
    fvars(M, V1),
    fvars(N, V2),
    union(V1, V2, V0).    
fvars(mul(M, N), V0) :-
    fvars(M, V1),
    fvars(N, V2),
    union(V1, V2, V0).    
fvars(true, []).
fvars(false, []).
fvars(not(E), V) :-
    fvars(E, V).
fvars(null(E), V) :-
    fvars(E, V).
fvars(if(E1, E2, E3), V0) :-
    fvars(E1, V1),
    fvars(E2, V2),
    fvars(E3, V3),
    union(V1, V2, V4),
    union(V4, V3, V0).


%
% Small-step semantics cont'd
%
step(add(M1, N), add(M2, N)) :-
    step(M1, M2).
step(add(M, N1), add(M, N2)) :-
    normal(M),
    step(N1, N2).
step(add(number(N1), number(N2)), number(N0)) :-
    N0 is N1 + N2.
step(mul(M1, N), mul(M2, N)) :-
    step(M1, M2).
step(mul(M, N1), mul(M, N2)) :-
    normal(M),
    step(N1, N2).
step(mul(number(N1), number(N2)), number(N0)) :-
    N0 is N1 * N2.
step(not(M), not(N)) :-
    step(M, N).
step(not(true), false).
step(not(false), true).
step(null(M), null(N)) :-
    step(M, N).
step(null(number(N)), V) :-
    N == 0 -> V = true; V = false.
step(if(M, E1, E2), if(N, E1, E2)) :-
    step(M, N).
step(if(true, E, _), E).
step(if(false, _, E), E).


%
% Substitution cont'd
%
subst(_, _, number(N), number(N)).
subst(X, N, add(M1, M2), add(M3, M4)) :-
    subst(X, N, M1, M3),
    subst(X, N, M2, M4).
subst(X, N, mul(M1, M2), mul(M3, M4)) :-
    subst(X, N, M1, M3),
    subst(X, N, M2, M4).
subst(_, _, true, true).
subst(_, _, false, false).
subst(X, N, not(M1), not(M2)) :-
    subst(X, N, M1, M2).
subst(X, N, null(M1), null(M2)) :-
    subst(X, N, M1, M2).
subst(X, N, if(M1, M2, M3), if(N1, N2, N3)) :-
    subst(X, N, M1, N1),
    subst(X, N, M2, N2),
    subst(X, N, M3, N3).


%
% Call-by-value fixed point combinator as a lambda term
% (\x. \y. (y (\z. x x y z))) (\x. \y. (y (\z. x x y z)))
% http://en.wikipedia.org/wiki/Fixed-point_combinator
%
fix(apply(T, T)) :-
    T = lambda(x,
	       lambda(y,
		      apply(
			      var(y),
			      lambda(z, apply(apply(apply(var(x), var(x)), var(y)), var(z)))))).

%
% A new sample term
%   Let's test the number 777 to be even
%   In Haskell syntax: fix (\f x -> if x==0 then True else not (f (x-1))) 777
%
sample(
	apply(
		apply(
			Fix,
			lambda(f,
			       lambda(x,
				      if(
					      null(var(x)),
					      true,
					      not(apply(var(f), add(var(x), number(-1)))))))),
		number(777))
) :-
      fix(Fix).



/*

% Demo reductions

?- reduce(add(number(1), number(2)), X).
X = number(3).

?- reduce(if(null(number(1)), number(1), number(2)), X).
X = number(2).

?- sample(M1), reduce(M1, M2).
M1 = apply(apply(apply(lambda(x, lambda(y, apply(var(y), lambda(z, apply(apply(apply(..., ...), var(...)), var(z)))))), lambda(x, lambda(y, apply(var(y), lambda(z, apply(apply(apply(..., ...), var(...)), var(z))))))), lambda(f, lambda(x, if(null(var(x)), true, not(apply(var(f), add(var(x), number(-1)))))))), number(777)),
M2 = false.

% Thus, 777 is indeed not even.

*/
