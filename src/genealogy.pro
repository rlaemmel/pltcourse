/* 

(C) 2011 Ralf Laemmel

The following source provided the family tree of Steve Jobs: 

http://www.applegazette.com/feature/the-family-tree-of-steve-jobs/

*/

:- multifile(sex/2).
:- multifile(father/2).
:- multifile(mother/2).

% Steve's adopted parents

sex(steve,male).
father(paul,steve).
mother(clara,steve).

% Steve's biological parents

father(abdul,steve).
mother(joanne,steve).

% Sister of Steve

sex(mona,female).
father(abdul,mona).
mother(joanne,mona).

% Steve's daughter back from his sterile period

sex(lisa,female).
father(steve,lisa).
mother(anne,lisa).

% Steve's kids from his non-sterile period

sex(reed,male).
father(steve,reed).
mother(laurene,reed).

sex(erin,female).
father(steve,erin).
mother(laurene,erin).

sex(eve,female).
father(steve,eve).
mother(laurene,eve).


% Genealogy relations

grandfather(X,Y) :-
  father(X,Z),
  father(Z,Y).

sibling(X,Y) :-
  father(F,X),
  father(F,Y),
  mother(M,X),
  mother(M,Y),
  X \== Y.

sister(X,Y) :-
  sibling(X,Y),
  sex(X,female).

halfsister(X,Y) :-
  sex(X,female),
  father(FX,X),
  mother(MX,X),
  father(FY,Y),
  mother(MY,Y),
  ( FX == FY, MX \== MY; FX \== FY, MX == MY ).

/*

% Is Steve the grandfather of someone?

?- grandfather(steve,X).
false.

% Do we know who Steve's grandfather is?

?- grandfather(X,steve).
false.

% Do we know who Reed's grandfather is?

?- grandfather(X,reed).
X = paul ;
X = abdul ;
false.

% Does Steve have a sibling?

?- sibling(steve,X).
X = mona ;
false.

% Does Steve have a sister?

?- sister(X,steve).
X = mona ;
false.

% Does Reed a halfsister?

?- halfsister(X,reed).
X = lisa .

% Does Steve a halfsister?
% (Is Mona really such a halfsister?)
% (Why does Mona apear twice?)

?- 
|    halfsister(X,steve).
X = mona ;
X = mona .

*/
