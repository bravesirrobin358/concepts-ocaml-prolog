filter_list([],_,[]):-!.

filter_list(_,[],_):-true.

filter_list([H|T],C,[H|L]):-
    works([H],C),
    filter_list(T,C,L),!.

filter_list([_|T],[H2|T2],L):-
    filter_list(T,[H2|T2],L).

works(_,[]):-true.
works(H,[H2|T]):-
    call(H2,H),
    works(H,T).

greater_than(Target,Value):-
    Target<Value.

multiple_of(Target,Value):-
    0 is Value mod Target.
