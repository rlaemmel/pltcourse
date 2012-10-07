/*

(C) 2011 Ralf Laemmel

Using univ (=..) to print terms with indentation.

*/

print_term(T) :-
  print_term(T,0).

print_term(T,N) :-
  spaces(N),
  ( var(T) -> 
       format('~w~n',[T])
     ; T =.. [F|Ts],
       format('~w~n',[F]),
       M is N + 1,
       print_terms(Ts,M) ).

print_terms([],_).

print_terms([H|T],N) :-
  print_term(H,N),
  print_terms(T,N).

spaces(N) :-
  N > 0 -> write(' '), M is N - 1, spaces(M); true.

varmember(V,[H|_]) :- V==H.
varmember(V,[H|T]) :- V\==H, varmember(V,T).

/*

?- print_term(add(num(1),add(num(2),num(3)))).
add
 num
  1
 add
  num
   2
  num
   3

?- print_term(add(num(1),add(num(X),num(X)))).
add
 num
  1
 add
  num
   _G253
  num
   _G253

*/
