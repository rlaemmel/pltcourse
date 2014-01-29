/*

The following variant of CCS covers these constructs:
- labels with bars and without
- tau/0 as the completed perfect action
- zero/0 as the completed or stuck process
- add/2 as nondeterministic choice
- comp/2 as concurrent composition

We define a step/3 relation. In step(E1, A, E2), E1 is the initial process, A is the resulting action, and E2 is the resulting process. We also define a many/3 relation which is the reflexive, transitive closure of step/3. In many(E1, T, E2), E is the initial process, T is a list of actions (which we also may call "trace"), and E2 is the final process. Ultimately, we define the traces/2 relation, which takes the closure of all possible traces.

*/

% Unbared and bared names
label(nobar(X)) :- atom(X).
label(bar(X)) :- atom(X).

% Actions
action(X) :- label(X).
action(tau). % Completed perfect action

% Negation of labels
negate(nobar(X), bar(X)).
negate(bar(X), nobar(X)).

% Agent / process expressions
expr(zero). % Special stuck process
expr(prefix(A, E)) :- action(A), expr(E).
expr(add(E1, E2)) :- expr(E1), expr(E2).
expr(comp(E1, E2)) :- expr(E1), expr(E2).

% Step for action of prefix
step(prefix(A, E), A, E).

% Step for nondeterministic choice 
step(add(E1, E2), A, Ej) :-
  member(Ei, [E1, E2]),
  step(Ei, A, Ej).

% Left step for composition
step(comp(E1,E2), A, comp(E3,E2)) :-
  step(E1, A, E3).

% Right step for composition
step(comp(E1,E2), A, comp(E1,E3)) :-
  step(E2, A, E3).

% Composition with communication
step(comp(E1, E2), tau, comp(E3, E4)) :-
  step(E1, A1, E3),
  step(E2, A2, E4),
  negate(A1, A2).

% No more steps
many(E, [], E) :-
  \+ step(E, _, _).

% One or more steps
many(E1, [H|T], E3) :-
  step(E1, H, E2),
  many(E2, T, E3).

% Compute all traces
traces(E, L) :-
  findall(T, many(E, T, _), L).

% Testing
main :-
  E1 = prefix(bar(a), zero),
  E2 = prefix(nobar(a), zero),
  E = comp(E1, E2), 
  expr(E),
  traces(E, L),
  format('Traces for ~q:~n~q~n', [E, L]).

/*

Illustration:

?- main.
Traces for comp(prefix(bar(a),zero),prefix(nobar(a),zero)):
[[bar(a),nobar(a)],[nobar(a),bar(a)],[tau]]
true.

Thus, there are 3 different traces for the simple composition.

*/
