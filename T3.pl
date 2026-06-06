next_to(_, _, []):- false.

next_to(X,Y,[X|[Y|_]]):- true.

next_to(X,Y,[_|T]):-
    next_to(X,Y,T).

not_next_to(X,Y,List):- not(next_to(X,Y,List)).

clue1(Houses):-
    next_to('Red','Blue',Houses).

clue2(['Green'|_], GreenIndex,GreenIndex).

clue2([_|T],GreenIndex,Val):-
    NewGreenIndex is Val+1,
    clue2(T,GreenIndex,NewGreenIndex).

clue3(Houses):-
    not_next_to('Yellow','Green',Houses),
    not_next_to('Green','Yellow',Houses).

clue4([_,'Green',_,_]):-false.

clue4(_):-true.


solve_puzzle(Houses, GreenIndex):-
    permutation(['Red','Yellow','Blue','Green'],Houses),
    clue1(Houses),
    clue2(Houses, GreenIndex,1),
    clue3(Houses),
    clue4(Houses).








