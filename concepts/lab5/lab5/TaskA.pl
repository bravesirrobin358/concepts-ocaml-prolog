/* Parent-child relationships */
parent(john, mary).
parent(john, tom).
parent(mary, ann).
parent(mary, fred).
parent(tom, liz).
/* Genders*/
male(john).
male(tom).
male(fred).
female(mary).
female(ann).
female(liz).

/* Question 1 */
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

/* Question 2 */
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

/* Question 3 */
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).


