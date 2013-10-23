%
% One-step relation of the CCS semantics
%

% Action

step((A,E),A,E).


% Choices for +

step(E0+_,A,E1) :- step(E0,A,E1).
step(_+E0,A,E1) :- step(E0,A,E1).


% Composition

step(E1|E2,A,E3|E2) :- step(E1,A,E3).
step(E1|E2,A,E1|E3) :- step(E2,A,E3).
step(E1|E2,tau,E3|E4) :-
  step(E1,A,E3),
  step(E2,B,E4),
  match(A,B).


% Inline agent according to equation

step(name(N),A,E1) :-
  equation(N,E0), 
  step(E0,A,E1).


% Actions fit for handshake

match(in(X),out(X)).
match(out(X),in(X)).
