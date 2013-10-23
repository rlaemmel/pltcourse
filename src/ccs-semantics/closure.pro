%
% Transitive closure of transition relation
%  We limit the length of the action sequence by length.
%  Otherwise we would get non-terminating behavior.
%

many(0,E,[],E).
many(X,E1,[A|L],E3) :-
  X > 0,
  step(E1,A,E2),
  Y is X - 1,
  many(Y,E2,L,E3).
