:- ['prelude.pro'].

/*

// This is a Java program used as a starting point

import java.util.Scanner;

public class Demo {
	
	// Compute factorial of a number
	public static void main(String[] args) {
		// Read argument from stdin
		Scanner in = new Scanner(System.in);
		// Actual computation of factorial
		int n = in.nextInt();
		int r = 1;
		while (n>0) r *= n--;
		// Write result to stdout
		System.out.println(r);
	}

}

// Let us simplify the language at hand.
// Here is the same program in a conceived language While.

int n; 
read n;
int r;
r = 1;
while (n>0) do 
  r = r * n;
  n = n - 1;
od;
write r;

// Let's agree on what are the new challenges compared to FL:
// * input/output (as one kind of side effect)
// * imperative variables (as another kind of side effect)
// * while loop (also intrinsically imperative)

*/

% The expression syntax needed for the example at hand
expr(intconst(I)) :- integer(I).
expr(var(X)) :- atom(X).
expr(binary(E1, E2, O)) :- expr(E1), expr(E2), op(O).

% Binary operators needed
op(greater).
op(mult).
op(sub).

% The statement syntax needed for the example at hand
stmt(intdecl(X)) :- atom(X).
stmt(read(X)) :- atom(X).
stmt(assign(X, E)) :- atom(X), expr(E).
stmt(while(E, S)) :- expr(E), stmt(S).
stmt(seq(Ss)) :- map(stmt, Ss).
stmt(write(E)) :- expr(E).

% An "Ok" sample (in the sense of well-formedness; see below)
okSample(
  seq([
    intdecl(n),
    read(n),
    intdecl(r),
    assign(r, intconst(1)),
    while(
      binary(var(n), intconst(0), greater),
      seq([
        assign(r, binary(var(r), var(n), mult)),
        assign(n, binary(var(n), intconst(1), sub))
      ])
    ),
    write(var(r))
  ])
).

% An "non-Ok" sample (in the sense of well-formedness; see below)
failSample(
  seq([
    assign(r, intconst(1))
  ])
).

% Expression evaluation
eval(_, intconst(I), I).
eval(State, var(X), V) :- member((X, V), State).
eval(State, binary(E1, E2, O), V) :-
  eval(State, E1, V1),
  eval(State, E2, V2),
  evalOp(V1, V2, O, V).

% Evaluation of binary operators
evalOp(V1, V2, greater, true) :- V1 > V2.
evalOp(V1, V2, greater, false) :- \+ V1 > V2.
evalOp(V1, V2, mult, V) :- V is V1 * V2.
evalOp(V1, V2, sub, V) :- V is V1 - V2.

% Execution semantics of statements
exec(intdecl(_), In, In, State, State, []).
exec(read(X), [V|In], In, State1, State2, []) :- 
  update(State1, X, V, State2).
exec(assign(X, E), In, In, State1, State2, []) :-
  eval(State1, E, V),
  update(State1, X, V, State2).
exec(while(E, _), In, In, State, State, []) :-
  eval(State, E, false).
exec(while(E, S), In1, In2, State1, State2, Out) :-
  eval(State1, E, true),
  exec(seq([S, while(E, S)]), In1, In2, State1, State2, Out).
exec(seq([]), In, In, State, State, []).
exec(seq([S|Ss]), In1, In3, State1, State3, Out) :-
  exec(S, In1, In2, State1, State2, Out1),
  exec(seq(Ss), In2, In3, State2, State3, Out2),
  append(Out1, Out2, Out).
exec(write(E), In, In, State, State, [V]) :-
  eval(State, E, V).

% Shortcut for semantics of top-level statements (programs)
prog(S, In, Out) :-
  exec(S, In, [], [], _, Out).

% Well-typedness of expressions
wellTyped(_, intconst(_), inttype).
wellTyped(Decls, var(X), inttype) :- 
  member(intdecl(X), Decls).
wellTyped(Decls, binary(E1, E2, O), T) :-
  wellTyped(Decls, E1, T1),
  wellTyped(Decls, E2, T2),
  wellTypedOp(T1, T2, O, T).

% Well-typedness of operator applications
wellTypedOp(inttype, inttype, greater, booltype).
wellTypedOp(inttype, inttype, mult, inttype).
wellTypedOp(inttype, inttype, sub, inttype).

% Well-formedness of statements
wellFormed(intdecl(X), Decls, [intdecl(X)|Decls]) :-
  \+ member(intdecl(X), Decls).
wellFormed(read(X), Decls, Decls) :-
  member(intdecl(X), Decls).
wellFormed(assign(X, E), Decls, Decls) :-
  member(intdecl(X), Decls),
  wellTyped(Decls, E, inttype).
wellFormed(while(E, S), Decls, Decls) :-
  wellTyped(Decls, E, booltype),
  wellFormed(S, Decls, _).
wellFormed(seq([]), Decls, Decls).
wellFormed(seq([S|Ss]), Decls1, Decls3) :-
  wellFormed(S, Decls1, Decls2),
  wellFormed(seq(Ss), Decls2, Decls3).
wellFormed(write(E), Decls, Decls) :-
  wellTyped(Decls, E, _).

% Lineup of sample, syntax check, well-formedness, execution
testrun :-
  okSample(S),
  stmt(S),
  wellFormed(S, [], _),
  prog(S, [5], Out),
  format('Output = ~w~n', [Out]).
