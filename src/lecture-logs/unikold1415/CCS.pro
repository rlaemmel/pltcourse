%
% Syntax of a simple CCS subset
%

% Names and co-names as well as tau

action(in(X)) :- atom(X).
action(out(X)) :- atom(X).
action(tau).

% Agent expressions

agent(0).  % Completed agent (empty sum)
agent((A,E)) :- action(A), agent(E).  % Action prefix
agent(E1+E2) :- action(E1), action(E2).  % non-deterministic choice
agent(E1|E2) :- action(E1), action(E2).  % composition
agent(name(N)) :- atom(N).  % name of agent defined by equation


%
% One-step relation of the CCS semantics
%

% Action
step((A,E),A,E).

% Choices for +
step(E0+_,A,E1) :- step(E0,A,E1).
step(_+E0,A,E1) :- step(E0,A,E1).

% Composition with communication to deal with SLD resolution
step(E1|E2,tau,E3|E4) :-
  step(E1,A,E3),
  step(E2,B,E4),
  match(A,B).
step(E1|E2,A,E3|E2) :- step(E1,A,E3).
step(E1|E2,A,E1|E3) :- step(E2,A,E3).

% Inline agent according to equation
step(name(N),A,E1) :-
  equation(N,E0), 
  step(E0, A, E1).
  
% Actions fit for handshake
match(in(X),out(X)).
match(out(X),in(X)).


%
% Let's define a normal form -- pragmatically
%

normal(0).
normal(E|_) :- normal(E).
normal(_|E) :- normal(E).


%
% Transitive closure of transition relation
%

many(E,[],E) :- normal(E).
many(E1,[A|L],E3) :-
  \+ normal(E1),
  step(E1,A,E2),
  many(E2,L,E3).


% 
% Agent equations for a chocolade purchase
%  machine is a vending machine.
%  customer is someone who purchases one big chocolade.
%

equation(
  purchase,
  (name(machine)|name(customer))).
  
equation(
  machine,
  ((in(p2),in(big),out(collect),name(machine))
  +(in(p1),in(little),out(collect),name(machine)))).

equation(
  customer,
  (out(p2),out(big),in(collect),0)).


/*

% Demo

?- many(name(purchase),L,X).
L = [tau, tau, tau],
X = (name(machine)| 0) .

*/
