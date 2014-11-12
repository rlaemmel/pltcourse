/*

(C) 2014, Ralf Laemmel


Forq background on big-step operational semantics (or natural semantics) 
and small-step operational semantics (or structured operational semantics), 
please consider chapter 2 of http://www.daimi.au.dk/~bra8130/Wiley_book/wiley.pdf

Let's define abstract syntax, (dynamic) semantics, and a type system
for a tiny language. We are going to play with a stack-based language,
somewhat inspired by Forth. Let's call it MiniForth. As to the
semantics, we will be doing both big-step and small-step operational
semantics (SOS). As to the type system, we will be using types to
model the pre- and postconditions of programs with regard to stack
size.

A sample MiniForth program

seq(dup, mult)

The program squares the top of stack (TOS).

*/

/* Abstract syntax */

op(skip).
op(seq(O1, O2)) :- op(O1), op(O2).
op(push(I)) :- integer(I).
op(pop).
op(dup).
op(add).
op(mult).

/*

Alternative notation as a grammar:

op -> "skip"
   | op ";" op 
   | "push" integer
   | "pop" 
   | "dup"
   | "add"
   | "mult"

*/

/* Big-step operational semantics */

bigstep(skip, S, S).
bigstep(seq(O1, O2), S1, S3) :-
  bigstep(O1, S1, S2),
  bigstep(O2, S2, S3).
bigstep(push(I), S, [I|S]).
bigstep(pop, [_|S], S).
bigstep(dup, [I|S], [I,I|S]).
bigstep(add, [I1,I2|S], [I3|S]) :- I3 is I1 + I2.
bigstep(mult, [I1,I2|S], [I3|S]) :- I3 is I1 * I2.

/*

% Some bits of testing

?- bigstep(seq(dup, mult), [], S).
false.

?- bigstep(seq(dup, mult), [2], S).
S = [4].

*/

/* Small-step operational semantics */

smallstep(seq(skip, O), O, S, S).
smallstep(seq(O1a, O2), seq(O1b, O2), S1, S2) :-
  smallstep(O1a, O1b, S1, S2).
smallstep(pop, skip, [_|S], S).
smallstep(dup, skip, [I|S], [I,I|S]).
smallstep(add, skip, [I1,I2|S], [I3|S]) :- I3 is I1 + I2.
smallstep(mult, skip, [I1,I2|S], [I3|S]) :- I3 is I1 * I2.

/* Reflexive, transitive closure */

allsteps(O1, S1, S3) :-
  smallstep(O1, O2, S1, S2) ->
    allsteps(O2, S2, S3) ;
    S3 = S1.

/*

% Some bits of testing

?- smallstep(seq(dup, mult), X, [2], S).
X = seq(skip, mult),
S = [2, 2].

?- allsteps(seq(dup, mult), [2], S).
S = [4].

*/

/*

Types are of the following format:

(X, Y)
  - X is minimum length of initial stack
  - Y is difference in length of final stack

*/

typeOf(skip, (0, 0)).
typeOf(push(_), (0, 1)).
typeOf(pop, (1, -1)).
typeOf(dup, (1, 1)).
typeOf(add, (2, -1)).
typeOf(mult, (2, -1)).
typeOf(seq(O1, O2), (Min3, Diff3)) :-
  typeOf(O1, (Min1, Diff1)),
  typeOf(O2, (Min2, Diff2)),
  Min3 is max(Min1, Min2-Diff1),
  Diff3 is Diff1 + Diff2.

/*

% Some bits of testing

?- typeOf(seq(dup, mult), X).
X = (1, 0).

?- typeOf(seq(pop,pop), X).
X = (2, -2).

?- typeOf(seq(push(42),pop), X).
X = (0, 0).

*/
