% Reuse most of SortOfWhile
:- multifile stm/1.
:- multifile aexp/1.
:- multifile bexp/1.
:- multifile aeval/3.
:- multifile beval/3.
% :- multifile exec/3. % Predicate needs an extra argument
:- ['SortOfWhile.pro'].

/*

A sample program in concrete syntax

begin
    var x := 0;
    proc p is x := x * 2;
    proc q is call p;
    begin 
        var x := 5;
        proc p is x := x + 1;
        call q; y := x;
    end
end

Option 1: Dynamic scope for variables and procedures

Execution trace

* x := 0;
* x := 5;
* call q;
  * call inner.p;
    * x := x + 1; // x := 6;
* y := x; // y := 6;

Option 2: Dynamic scope for variables; static scope for procedures

See StaticProc.pro

*/

% Syntax extensions for procedure calls and block statements
stm(call(I)) :-
    atom(I).
stm(block(Dv, Dp, S)) :-
    dv(Dv),
    dp(Dp),
    stm(S).

% Block declarations for variables
dv(nodv).
dv(dvs(I, E, Dv)) :-
    atom(I),
    aexp(E),
    dv(Dv).

% Block declarations for procedures
dp(nodp).
dp(dps(I, S, Dp)) :-
    atom(I),
    stm(S),
    dp(Dp).

% Big-step semantics of SortOfWhile with extra environment argument
exec(skip, _, M, M).
exec(seq(S1, S2), C, M1, M3) :-
    exec(S1, C, M1, M2),
    exec(S2, C, M2, M3).
exec(assign(I, E), _, M1, M2) :-
    aeval(E, M1, V),
    update(M1, I, V, M2).
exec(if(E, S1, _), C, M1, M2) :-
    beval(E, M1, tt),
    exec(S1, C, M1, M2).
exec(if(E, _, S2), C, M1, M2) :-
    beval(E, M1, ff),
    exec(S2, C, M1, M2).
exec(while(E, S), C, M1, M2) :-
    beval(E, M1, tt),
    exec(seq(S, while(E, S)), C, M1, M2).
exec(while(E, _), _, M, M) :-
    beval(E, M, ff).

% Semantics of procedure call
exec(call(I), C, M1, M2) :-
    % Look up body of called method
    lookup(C, I, S),
    exec(S, C, M1, M2).

% Semantics of block statements
exec(block(Dv, Dp, S), C1, M1, M3) :-
    upddv(Dv, M1, M2),
    upddp(Dp, C1, C2),
    exec(S, C2, M2, M3).

% Semantics of variable declarations
upddv(nodv, M, M).
upddv(dvs(I, E, Dv), M1, M3) :-
    aeval(E, M1, V),
    update(M1, I, V, M2),
    upddv(Dv, M2, M3).

% Semantics of procedure declarations
upddp(nodp, C, C).
upddp(dps(I, S, Dp), C1, C3) :-
    % Map procedure name to body
    update(C1, I, S, C2),
    upddp(Dp, C2, C3).

% sample/1 declares the illustrative sample program
sample(
 block(
  dvs(x, number(0), nodv),
  dps(p, assign(x, mul(var(x), number(2))),
  dps(q, call(p), nodp)),
  block(
    dvs(x, number(5), nodv),
    dps(p, assign(x, add(var(x), number(1))), nodp),
    seq(
      call(q),
      assign(y, var(x)))))).

/*

% Interpretation of the sample program

?- sample(S), stm(S), exec(S, [], [], M).
S = block(dvs(x, number(0), nodv), dps(p, assign(x, mul(var(x), number(2))), dps(q, call(p), nodp)), block(dvs(x, number(5), nodv), dps(p, assign(x, add(var(x), number(1))), nodp), seq(call(q), assign(y, var(x))))),
M = [ (y, 6), (x, 6)] .

*/
