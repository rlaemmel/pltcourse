/*

Some helper predicates

*/

% Mapping a predicate over a list
map(_, []).
map(P, [H|T]) :-
  apply(P, [H]),
  map(P, T).

% ... also compute a result lists
map(_, [], []).
map(P, [H1|T1], [H2|T2]) :- 
  apply(P, [H1, H2]),
  map(P, T1, T2).

% Zip two lists together
zip([], [], []).
zip([H1|T1], [H2|T2], [(H1, H2)|T12]) :-
  zip(T1, T2, T12).

% Update a dictionary for one key
update([], X, Y, [(X, Y)]).
update([(X, _)|M], X, Y, [(X, Y)|M]).
update([(X1, Y1)|M1], X2, Y2, [(X1, Y1)|M2]) :-
  X1 \= X2,
  update(M1, X2, Y2, M2).



