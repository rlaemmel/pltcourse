/* 

(C) 2011 Ralf Laemmel

The implementation of a very simply imperative programming language.
We use Prolog terms for a readable syntax.
We interpret programs in a style inspired by big-step operational semantics.

*/

program(Es) :- exprs(Es).

exprs([]).
exprs([E|Es]) :- expr(E), exprs(Es).

expr(N) :- number(N).
expr(E1+E2) :- expr(E1), expr(E2).
expr(V) :- atom(V).
expr(V=E) :- atom(V), expr(E).

eval(Es,V) :- eval(Es,V,[],_).

eval([E],N,M1,M2) :- eval(E,N,M1,M2).
eval([E|Es],N,M1,M2) :- Es \== [], eval(E,_,M1,M0), eval(Es,N,M0,M2).
eval(N,N,M,M) :- number(N).
eval(E1+E2,N,M1,M2) :- eval(E1,N1,M1,M0), eval(E2,N2,M0,M2), N is N1+N2.
eval(V,N,M,M) :- atom(V), lookup(V,M,N).
eval(V=E,N,M1,M2) :- atom(V), eval(E,N,M1,M0), update(V,N,M0,M2).

lookup(V,[(V,N)|_],N).
lookup(V,[(W,_)|R],N) :- V \== W, lookup(V,R,N).

update(V,N,[],[(V,N)]).
update(V,N,[(V,_)|R],[(V,N)|R]).
update(V,N,[(W,M)|R],[(W,M)|S]) :- V \== W, update(V,N,R,S).

/*

?- program([x=1,y=x+41]).
true 

?- eval([x=1,y=x+41],N).
N = 42

*/
