:- ['SortOfWhile.pro'].

/*

(C) 2014, Ralf Laemmel

A simple proof checker for axiomatic semantics (say, Hoare logic). 

See here for background:

* Chapter 6 of Nielson&Nielson: "Semantics with applications":
  http://www.daimi.au.dk/~bra8130/Wiley_book/wiley.pdf

* Wikipedia article on Hoare logic:
  http://en.wikipedia.org/wiki/Hoare_logic

* Wikipedia article on automated proof checking
  http://en.wikipedia.org/wiki/Automated_proof_checking

*/

/* In the case of the empty statement, pre- and postcondition are
 * equivalent.
 */

proof(P, skip, Q) :-
    sameAs(P, Q).


/* In the case of an assignment, its precondition is calculated from
 * its postcondition by substituting the LHS variable by the RHS
 * expression.
 */

proof(P, assign(X, E), Q) :-
    subst(Q, X, E, P).

/* In the case of a statement sequence seq(S1, S2), the proof requires
 * subproofs for S1 and S2 subject to an intermediate assertion that
 * serves as postcondition of S1 and precondition of S2. To this end,
 * a statement sequence is represented here as a term with functor
 * seq_/3 (as opposed to the functor seq/2 of the original abstract
 * syntax) so that the extra intermediate assertion can be expressed.
 */

proof(P, seq_(S1, R, S2), Q) :-
    proof(P, S1, R),
    proof(R, S2, Q).

/* In the case of an if-statement if(E, S1, S2), two subproofs are
 * due; one for the then-branch S1; another for the else-branch
 * S2. The preconditions of the branches are obtained by adding the
 * conjunctive operand E or not(E), respectively, to the precondition
 * of the if-statement. The postcondition of the if-statement applies
 * to both branches.
 */

proof(P, if(E, S1, S2), Q) :-
    proof(and(P, E), S1, Q),
    proof(and(P, not(E)), S2, Q).

/* In the case of a while-loop while(E, S), the precondition of the
 * loop is per definition the invariant I of the loop and the
 * postcondition must be equivalent to and(I, not(E)). A subproof is
 * due for the body S of the loop such that the invariant follows as
 * postcondition of S for the precondition and(I, E), i.e., the
 * invariant and the loop's condition hold at the beginning of the
 * execution of the body.
 */

proof(I, while(E, S), Q) :-
    sameAs(and(I, not(E)), Q),
    proof(and(I, E), S, I).

/* The precondition of a proof can be strengthened. While
 * simplification/normalization of assertions is performed
 * automatically, strengthening must be explicitly requested. (This is
 * well in line with the usual extra rule of Hoare logic for
 * strengthening the precondition.) In the following notation, we
 * assume pseudo syntax to express the precondition before
 * strengthening, whereas the precondition of the triple is the
 * strengthened one.
 */

proof(P, pre(R, S), Q) :-
    implies(P, R),
    proof(R, S, Q).

/* See strengthening of preconditions above. This rule is for
 * weakening of postconditions instead.
 */

proof(P, post(R, S), Q) :-
    implies(R, Q),
    proof(P, S, R).


/* Substitution of variables in expressions */

% Actual substitution complemented by normalization

subst(E1, X, E2, E3) :-
    subst_(E1, X, E2, E4),
    sameAs(E3, E4).

% Top-down traversal for actual substitution

subst_(number(N), _, _, number(N)).
subst_(var(X), X, E, E).
subst_(var(X1), X2, _, var(X1)) :-
    \+ X1 == X2.
subst_(E1, X, E0, E2) :-
    E1 =.. [F, E1a],
    E2 =.. [F, E2a],
    % Unary operators
    member(F, [not]),
    subst_(E1a, X, E0, E2a).
subst_(E1, X, E0, E2) :-
    E1 =.. [F, E1a, E1b],
    E2 =.. [F, E2a, E2b],
    % Binary operators
    member(F, [eq, greater, geq, or, add, sub, mul, and, or]),
    subst_(E1a, X, E0, E2a),
    subst_(E1b, X, E0, E2b).


/* Simplification of expressions */

% Test two expressions to be the same under normalization
sameAs(E1, E2) :-
    normalize(E1, E3),
    normalize(E2, E3).

% Reflexive, transitive closure
normalize(E1, E3) :-
    rewrite(E1, E2) ->
	normalize(E2, E3)
      ; E3 = E1.

% Congruence rule for unary operators
rewrite(E1, E2) :-
    E1 =.. [F, E1a],
    E2 =.. [F, E2a],
    member(F, [not]),
    rewrite(E1a, E2a).

% Congruence rule for binary operators (first operand)
rewrite(E1, E2) :-
    E1 =.. [F, E1a, E1b],
    E2 =.. [F, E2a, E1b],
    member(F, [eq, greater, geq, or, add, sub, mul, and, or]),
    rewrite(E1a, E2a).

% Congruence rule for binary operators (second operand)
rewrite(E1, E2) :-
    E1 =.. [F, E1a, E1b],
    E2 =.. [F, E1a, E2b],
    member(F, [eq, greater, geq, or, add, sub, mul, and, or]),
    \+ rewrite(E1a, _),
    rewrite(E1b, E2b).

% Reflexivity
rewrite(eq(X, X), true).
rewrite(geq(X, X), true).

% Prefer right-associativity over left-associativity
rewrite(and(and(E1, E2), E3), and(E1, and(E2, E3))).
rewrite(or(or(E1, E2), E3), or(E1, or(E2, E3))).

% Unit laws
rewrite(and(true, E), E).
rewrite(and(E, true), E).
rewrite(or(false, E), E).
rewrite(or(E, false), E).
rewrite(add(number(0), X), X).
rewrite(add(X, number(0)), X).

% Zero laws
rewrite(and(false, _), false).
rewrite(and(_, false), false).
rewrite(or(true, _), true).
rewrite(or(_, true), true).
rewrite(mul(number(0), _), number(0)).
rewrite(mul(_, number(0)), number(0)).

% Prefer greater over geq
rewrite(geq(X, number(N1)), greater(X, number(N2))) :-
    N2 is N1 - 1.

% Simplify conjunctions of comparisons
rewrite(and(geq(X, Y), greater(X, Y)), greater(X, Y)).
rewrite(and(geq(X, Y), not(greater(X, Y))), eq(X, Y)).

% Constant folding

rewrite(
	greater(number(N1), number(N2)),
	B) :-
    N1 > N2 -> B = true; B = false.

rewrite(
	geq(number(N1), number(N2)),
	B) :-
    N1 >= N2 -> B = true; B = false.

rewrite(
	greater(sub(X, number(N1)), number(N2)),
	greater(X, number(N0))) :-
    N0 is N1 + N2.

rewrite(
	and(greater(X, number(N1)), not(greater(X, number(N2)))),
	eq(X, number(N2))) :-
    N2 is N1 + 1.

rewrite(
	and(greater(X, number(N1)), greater(X, number(N2))),
	greater(X, number(N0))) :-
    N0 is max(N1, N2).

rewrite(
	greater(add(X, number(N1)), number(N2)),
	greater(X, number(N0))) :-
    N0 is N2 - N1.

rewrite(
	geq(sub(X, Y), number(0)),
	geq(X, Y)).

rewrite(
	greater(sub(X, Y), number(-1)),
	geq(X, Y)).

rewrite(
	add(mul(add(X, number(1)), Y), sub(Z, Y)),
	add(mul(X, Y), Z)).


/* Logical implication for Boolean expressions (assertions) */    

% Implication complemented by normalization
implies(E1, E2) :-
    normalize(E1, E3),
    normalize(E2, E4),
    implies_(E3, E4).

% Search rules for implication
implies_(E, E).
implies_(_, true).
implies_(and(E1, E2), and(E3, E4)) :-
    implies_(E1, E3),
    implies_(E2, E4).
implies_(and(E1, _), E2) :-
    implies_(E1, E2).
implies_(and(_, E1), E2) :-
    implies_(E1, E2).
implies_(greater(X, number(N1)), greater(X, number(N2))) :-
    N1 > N2.

	    
/* Testing proof checking */

% Simple test reporting
test(X) :-
    ( X -> R = true; R = fail ),
    format('~w ~w.~n', [X, R]),
    R.

% Proof for an empty statement
prove_skip :-
    proof(true, skip, true).

% Proof for a simple assignment
prove_assign :-
    proof(
	    eq(var(y), number(42)),
	    assign(x, var(y)),
	    eq(var(x), number(42))
    ).

% Proof for a sequence of assignments
prove_seq :-
    proof(
	    greater(var(x), number(4)),
	    seq_(
		    assign(x, sub(var(x), number(1))),
		    greater(var(x), number(3)),
		    assign(x, sub(var(x), number(1)))
	    ),
	    greater(var(x), number(2))
    ).

% Proof for a simple if-statement that computes the max of a and b
prove_if :-
    proof(
	    true,
	    if(
		    greater(var(a), var(b)),
		    pre(true, assign(r, var(a))),
		    pre(true, assign(r, var(b)))
            ),
	    or(eq(var(r), var(a)), eq(var(r), var(b)))
    ).

% Proof for a simple while-statement
prove_while :-
    proof(
	    geq(var(x), number(0)),
	    while(
		    greater(var(x), number(0)),
		    assign(x, sub(var(x), number(1)))
	    ),
	    eq(var(x), number(0))	  
    ).


/*

% Concrete syntax for Eucledian division -- for comparison

q = 0;
r = x;
while (r >= y) {
   r = r - y;
   q = q + 1; 
}

*/


% Euclidean division in SortOfWhile abstract syntax

sample(
  seq(
    assign(q, number(0)),	  
    seq(
      assign(r, var(x)),
      while(
        geq(var(r), var(y)),
        seq(
          assign(r, sub(var(r), var(y))),
          assign(q, add(var(q), number(1)))))))
).


% Run Eucledian division on a particular example; test output
run_div
 :-
     sample(S),
     many(S, [(x, 13), (y, 4)], V),
     member((q, 3), V),
     member((r, 1), V).


% Proof for Euclidean division
prove_div :-
    proof(

      % "As declared" precondition of the program
      % x >= 0 && y > 0
      and(
        geq(var(x), number(0)), % As computed
        greater(var(y), number(0))), % Vacuously added for division by zero

      % Strengthen precondition
      pre(

        % Computed (weakest) precondition of the program
        geq(var(x), number(0)),

        % Beginning of program code
        seq_(

          % q = 0;
          assign(q, number(0)),	  

          % Intermediate assertion
          and(
            eq(var(x), add(mul(var(q), var(y)), var(x))),
            and(
              geq(var(q), number(0)),
              geq(var(x), number(0)))),
		  
          seq_(

            % r = x;
            assign(r, var(x)),
		  
            % Intermediate assertion = invariant for while loop
            and(
              eq(var(x), add(mul(var(q), var(y)), var(r))),
              and(
                geq(var(q), number(0)),
                geq(var(r), number(0)))),

            while(
			  
              % Loop condition                     
              geq(var(r), var(y)),
			  
              % Strengthen precondition
              pre(
    
                % Computed precondition of body
                and(
                  eq(var(x), add(mul(add(var(q), number(1)), var(y)), sub(var(r), var(y)))),
                  and(
                    geq(add(var(q), number(1)), number(0)),
                    geq(sub(var(r), var(y)), number(0)))),
		  
                % Loop body
                seq_(
			  
                  % r = r - y;
                  assign(r, sub(var(r), var(y))),

                  % Intermediate assertion
                  and(
                    eq(var(x), add(mul(add(var(q), number(1)), var(y)), var(r))),
                    and(
                      geq(add(var(q), number(1)), number(0)),
                      geq(var(r), number(0)))),
			  
                  % q = q + 1;
                  assign(q, add(var(q), number(1))))))))),

      % "As declared" postcondition of the program
      % x == q*y + r && q >= 0 && r>0 && r < y
      and(
        eq(var(x), add(mul(var(q), var(y)), var(r))),
        and(
          geq(var(q), number(0)),
          and(
            geq(var(r), number(0)),
            not(geq(var(r), var(y))))))
    ).


% Run all the tests
main
 :-
    test(prove_skip),
    test(prove_assign),
    test(prove_seq),
    test(prove_if),
    test(prove_while),
    test(run_div),
    test(prove_div).


/*

% This is how it should look like at the prompt.

?- main.
prove_skip true.
prove_assign true.
prove_seq true.
prove_if true.
prove_while true.
run_div true.
prove_div true.
true.

*/
