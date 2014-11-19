/* 

(C) 2014, Ralf Laemmel

A Nielson&Nielson-like While language 

The While language with its natural (big-step) and structured
operational (i.e., small-step) semantics is covered in chapter 2 of
http://www.daimi.au.dk/~bra8130/Wiley_book/wiley.pdf Said chapter also
covers a section with extensions of While including nondeterminism and
parallelism as covered below ("par" and "or").

*/

/* Syntax of SortOfWhile */

% Statements

stm(skip).
stm(seq(S1, S2)) :- stm(S1), stm(S2).
stm(assign(I, E)) :- atom(I), aexp(E).
stm(if(E, S1, S2)) :- bexp(E), stm(S1), stm(S2).
stm(while(E, S)) :- bexp(E), stm(S).
stm(or(S1, S2)) :- stm(S1), stm(S2).
stm(par(S1, S2)) :- stm(S1), stm(S2).
stm(abort).


% Arithmetic expressions

aexp(number(N)) :- number(N).
aexp(var(I)) :- atom(I).
aexp(sub(E1, E2)) :- aexp(E1), aexp(E2).
aexp(div(E1, E2)) :- aexp(E1), aexp(E2).


% Boolean expressions

bexp(true).
bexp(false).
bexp(nonzero(E)) :- aexp(E).


/* Big-step (i.e., natural) semantics of SortOfWhile */

exec(skip, M, M).
exec(seq(S1, S2), M1, M3) :-
    exec(S1, M1, M2),
    exec(S2, M2, M3).
exec(assign(I, E), M1, M2) :-
    aeval(E, M1, V),
    update(M1, I, V, M2).
exec(if(E, S1, _), M1, M2) :-
    beval(E, M1, tt),
    exec(S1, M1, M2).
exec(if(E, _, S2), M1, M2) :-
    beval(E, M1, ff),
    exec(S2, M1, M2).
exec(while(E, S), M1, M2) :-
    beval(E, M1, tt),
    exec(seq(S, while(E, S)), M1, M2).
exec(while(E, _), M, M) :-
    beval(E, M, ff).
exec(or(S1, _), M1, M2) :- exec(S1, M1, M2).
exec(or(_, S2), M1, M2) :- exec(S2, M1, M2).
exec(par(S1, S2), M1, M2) :- exec(seq(S1, S2), M1, M2).
exec(par(S1, S2), M1, M2) :- exec(seq(S2, S1), M1, M2).

aeval(number(N), _, N).
aeval(var(I), M, V) :- append(_, [(I, V)|_], M).
aeval(sub(E1, E2), M, V3) :-
    aeval(E1, M, V1),
    aeval(E2, M, V2),
    V3 is V1 - V2.
aeval(div(E1, E2), M, V3) :-
    aeval(E1, M, V1),
    aeval(E2, M, V2),
    \+ V2 == 0, 
    V3 is V1 / V2.

beval(true, _, tt).
beval(false, _, ff).
beval(nonzero(E), M, V) :-
    aeval(E, M, 0) ->
	V = ff; V = tt.


% Helper for store update

update(M1, I, V, M2) :-
    append(M1a, [(I, _)|M1b], M1) ->
	append(M1a, [(I, V)|M1b], M2);
        M2 = [(I, V)|M1].


/* 

% Testing big-step semantics

?- exec(seq(assign(x,number(42)), assign(x, number(88))), [], X).
X = [ (x, 88)].

?- exec(seq(assign(x,number(42)), while(nonzero(var(x)), assign(x, sub(var(x), number(1))))), [], X).
X = [ (x, 0)].

?- exec(par(assign(x, number(1)), seq(assign(x, number(2)), assign(x, sub(var(x), number(-2))))), [], X).
X = [ (x, 4)] ;
X = [ (x, 1)] ;
false.

*/


/* Small-step (i.e., structured operational) semantics of SortOfWhile */

step(seq(skip, S), M, S, M).
step(seq(S1, S2), M1, seq(S3, S2), M2) :-
    step(S1, M1, S3, M2).
step(assign(I, E), M1, skip, M2) :-
    aeval(E, M1, V),
    update(M1, I, V, M2).
step(if(E, S1, _), M, S1, M) :-
    beval(E, M, tt).
step(if(E, _, S2), M, S2, M) :-
    beval(E, M, ff).
step(while(E, S), M, if(E, seq(S, while(E, S)), skip), M).
step(or(S1, _), M, S1, M).
step(or(_, S2), M, S2, M).
step(par(skip, S), M, S, M).
step(par(S, skip), M, S, M) .
step(par(S1, S2), M1, par(S3, S2), M2) :-
    step(S1, M1, S3, M2).
step(par(S1, S2), M1, par(S1, S3), M2) :-
    step(S2, M1, S3, M2).


/* Final state or normal form */

final(skip).


/*

% Closure for final store (with problematic cut)

many(S1, M1, M3) :-
    step(S1, M1, S2, M2) ->
	many(S2, M2, M3)
    ; M3 = M1.
*/


% Closure for final store

many(S1, M1, M3) :-
    step(S1, M1, S2, M2),
    many(S2, M2, M3).
many(S, M, M) :-
    final(S).


% Closure exposing final statement

many(S1, M1, S3, M3) :-
    step(S1, M1, S2, M2),
    many(S2, M2, S3, M3).
many(S, M, S, M) :-
    \+ step(S, M, _, _).


/*

% Testing small-step semantics

?- many(seq(assign(x,number(42)), assign(x, number(88))), [], X).
X = [ (x, 88)].

?- many(seq(assign(x,number(42)), while(nonzero(var(x)), assign(x, sub(var(x), number(1))))), [], X).
X = [ (x, 0)].

?- many(par(assign(x, number(1)), seq(assign(x, number(2)), assign(x, sub(var(x), number(-2))))), [], X).
X = [ (x, 4)] ;
X = [ (x, 4)] ;
X = [ (x, 4)] ;
X = [ (x, 4)] ;
X = [ (x, 4)] ;
X = [ (x, 3)] ;
X = [ (x, 3)] ;
X = [ (x, 3)] ;
X = [ (x, 3)] ;
X = [ (x, 3)] ;
X = [ (x, 3)] ;
X = [ (x, 3)] ;
X = [ (x, 1)] ;
X = [ (x, 1)] ;
X = [ (x, 1)] ;
false.

*/
