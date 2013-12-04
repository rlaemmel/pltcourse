% Syntax of the lambda calculus
term(var(X)) :- atom(X).
term(apply(T1, T2)) :- term(T1), term(T2).
term(lambda(X, T)) :- atom(X), term(T).

% Free variables in a term
fv(var(X), [X]).
fv(apply(T1, T2), FV0) :-
  fv(T1, FV1),
  fv(T2, FV2),
  union(FV1, FV2, FV0).
fv(lambda(X, T), FV0) :-
  fv(T, FV1),
  subtract(FV1, [X], FV0).

% A closed term (usually assumed for top-level terms)
cterm(T) :-
  term(T),
  fv(T, []).

% Values ("normal forms")
value(lambda(_, _)).

%
% CBV SOS for the lambda calculus
%

% Make a step for the function position
eval(apply(TF1, TA), apply(TF2, TA)) :-
  eval(TF1, TF2).

% Make a step for the argument position
eval(apply(V, TA1), apply(V, TA2)) :-
  value(V),
  eval(TA1, TA2).
   
% Perform beta reduction (substitution)
eval(apply(lambda(X, T1), V), T2) :-
  value(V),
  substitute(T1, X, V, T2).

%
% Substitution of a variable by a term within a term
%

% Replace the variable indeed
substitute(var(X), X, T, T).

% Retain different variables
substitute(var(X), Y, _, var(X)) :- X \= Y.

% Push substitution into subterms
substitute(apply(TF1, TA1), X, T, apply(TF2, TA2)) :-
  substitute(TF1, X, T, TF2),
  substitute(TA1, X, T, TA2).

% Stop substitution if variable of interest is rebound
substitute(lambda(X, T), X, _, lambda(X, T)). 

% Substitute in the body of the lambda if there is no capture
substitute(lambda(X, T1a), Y, T2, lambda(X, T1b)) :-
  X \= Y,
  fv(T2, FV),
  \+ member(X, FV),
  substitute(T1a, Y, T2, T1b).

% Perform alpha conversion in case of lambda capture
substitute(lambda(X, T1a), Y, T2, lambda(A, T1c)) :-
  X \= Y,
  fv(T2, FV2),
  member(X, FV2),
  fv(T1a, FV1),
  gensym(FV1, A),
  substitute(T1a, X, var(A), T1b),
  substitute(T1b, Y, T2, T1c).

% Generate a fresh symbol
gensym(L, X) :-
  atomic_list_concat(['_'|L], X).

%
% Test Church numerals
%
test_church :-

  % Church Numeral "0"
  C0 = lambda(s, lambda(z, var(z))),

  % Additional Church numerals not used in the illustration: 
  % C1 = lambda(s, lambda(z, apply(var(s), var(z)))),
  % C2 = lambda(s, lambda(z, apply(var(s), apply(var(s), var(z))))),

  % The increment function
  Inc = lambda(n, lambda(s, lambda(z, 
         apply(var(s), apply(apply(var(n), var(s)), var(z)))))),

  % Apply Inc to C0; this yields a term equivalent to C1
  T = apply(Inc, C0),
  eval(T, V),
  write(V), nl.

/*

?- test_church.
lambda(s,lambda(z,apply(var(s),apply(apply(lambda(s,lambda(z,var(z))),var(s)),var(z)))))

*/


%
% Test substitution
% 
test_substitute :-
  T1 = lambda(z, apply(var(x), var(z))),
  substitute(T1, x, var(z), T2),
  write(T2), nl.

/*

?- test_substitute.
lambda(_xz,apply(var(z),var(_xz)))

*/
