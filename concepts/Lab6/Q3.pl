second_max(List,Result):-
    sort(List,Unique),
    length(Unique,C),
    C>1,
    last_two(Unique,Result),!.

second_max(_,"Error: List must contain at least two distinct elements.").

last_two([_|[H2|[H3|T]]],Res):-
    last_two([H2|[H3|T]],Res).

last_two([H1,_],H1).

