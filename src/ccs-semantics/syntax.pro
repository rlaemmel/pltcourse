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
