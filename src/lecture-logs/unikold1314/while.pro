:- ['prelude.pro'].

/*

// Java program as a starting point

import java.util.Scanner;

public class Demo {
	
	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		int n = in.nextInt();
		int r = 1;
		while (n>0) r *= n--;
		System.out.println(r);
	}

}

// Simplified imperative language named While

int n; 
read n;
int r;
r = 1;
while (n>0) do 
  r = r * n;
  n = n - 1;
od;
write r;

// New challenges compared to FL
// * input/output (as one kind of side effect)
// * imperative variables (as another kind of side effect)
// * while loop (also intrinsically imperative)

*/

expr(intconst(I)) :- integer(I).
expr(var(X)) :- atom(X).
expr(binary(E1, E2, O)) :- expr(E1), expr(E2), op(O).

op(greater).
op(mult).
op(sub).

stmt(intdecl(X)) :- atom(X).
stmt(read(X)) :- atom(X).
stmt(assign(X, E)) :- atom(X), expr(E).
stmt(while(E, S)) :- expr(E), stmt(S).
stmt(seq(Ss)) :- map(stmt, Ss).
stmt(write(E)) :- expr(E).

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

failSample(
  seq([
    assign(r, intconst(1))
  ])
).

eval(_, intconst(I), I).
eval(State, var(X), V) :- member((X, V), State).
eval(State, binary(E1, E2, O), V) :-
  eval(State, E1, V1),
  eval(State, E2, V2),
  evalOp(V1, V2, O, V).

evalOp(V1, V2, greater, true) :- V1 > V2.
evalOp(V1, V2, greater, false) :- \+ V1 > V2.
evalOp(V1, V2, mult, V) :- V is V1 * V2.
evalOp(V1, V2, sub, V) :- V is V1 - V2.

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

prog(S, In, Out) :-
  exec(S, In, [], [], _, Out).

testrun :-
  okSample(S),
  stmt(S),
  wellFormed(S, [], _),
  prog(S, [5], Out),
  format('Output = ~w~n', [Out]).

% Time to do well-typedness!

wellTyped(_, intconst(_), inttype).
wellTyped(Decls, var(X), inttype) :- 
  member(intdecl(X), Decls).
wellTyped(Decls, binary(E1, E2, O), T) :-
  wellTyped(Decls, E1, T1),
  wellTyped(Decls, E2, T2),
  wellTypedOp(T1, T2, O, T).

wellTypedOp(inttype, inttype, greater, booltype).
wellTypedOp(inttype, inttype, mult, inttype).
wellTypedOp(inttype, inttype, sub, inttype).

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
