/*

(C) 2011 Ralf Laemmel

A DCG for a simple imperative language

*/

program --> expr, [;], rest.

rest --> [].
rest --> program.

expr --> [num(_)], add.
expr --> [id(_)], add.
expr --> [id(_)], [=], expr.

add --> [].
add --> [+], expr.

test :-
   program([id(x),=,num(1),;,id(y),=,id(x),+,num(41),;],[]).
