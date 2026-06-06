:- dynamic grid/2, revealed_grid/3, flagged_grid/2, mines/2.

fill_mines([]).
fill_mines([H|T]):-
    IntRem is div(H-1, 6)+1,
    Rem is mod(H-1,6)+1,
    assertz(mines(IntRem,Rem)),
    fill_mines(T).

fill_grid(37).

fill_grid(Pos):-
    IntRem is div(Pos-1, 6)+1,
    Rem is mod(Pos-1,6)+1,
    NextPos is Pos+1,
    assertz(grid(IntRem,Rem)),
    fill_grid(NextPos).

new_game():-
    randset(6,36,S),
    retractall(grid(_,_)),
    retractall(revealed_grid(_,_,_)),
    retractall(flagged_grid(_,_)),
    retractall(mines(_,_)),
    fill_mines(S),
    fill_grid(1),
    print_grid(),!.

reveal_cell(Row,Col):-
   mines(Row,Col),
   retract(grid(Row,Col)),
   assertz(revealed_grid(Row,Col,"*")),
   print_grid(),
   format("~nMine Found! Game over"),!.

reveal_cell(Row,Col):-
    revealed_grid(Row,Col,_),
    print("Cell already revealed"),!.

reveal_cell(Row,Col):-
    flagged_grid(Row,Col),
    print("Unflag cell first"),!.

reveal_cell(Row,Col):-
    grid(Row,Col),
    retract(grid(Row,Col)),
    count_adjacents(Row,Col,1,1,0,N),
    assert(revealed_grid(Row,Col,N)),
    print_grid(),!.

reveal_cell(_,_):-
    print("Invalid move").

count_adjacents(_,_,-1,-2,N,N).

count_adjacents(R,C,RR,-2,X,N):-
    RR1 is RR-1,
    count_adjacents(R,C,RR1,1,X,N).

count_adjacents(R,C,RR,RC,X,N):-
    NewR is R+RR,
    NewC is C+RC,
    mines(NewR,NewC),
    X1 is X+1,
    RC1 is RC-1,
    count_adjacents(R,C,RR,RC1,X1,N).

count_adjacents(R,C,RR,RC,X,N):-
    RC1 is RC-1,
    count_adjacents(R,C,RR,RC1,X,N).

print_grid():-
    get_grid(G,1,1),
    print_six(G).

print_six([]).
print_six([C1,C2,C3,C4,C5,C6|T]):-
    format('~n~w~w~w~w~w~w', [C1,C2,C3,C4,C5,C6]),
    print_six(T).	
    

get_grid([],7,_):-!.

get_grid(G,R,7):-
    R1 is R+1,
    get_grid(G,R1,1).

get_grid(["?"|G],R,C):-
    grid(R,C),
    C1 is C+1,
    get_grid(G,R,C1).

get_grid(["F"|G],R,C):-
    flagged_grid(R,C),
    C1 is C+1,
    get_grid(G,R,C1).

get_grid([N|G],R,C):-
    revealed_grid(R,C,N),
    C1 is C+1,
    get_grid(G,R,C1).

flag(R,C):-
    grid(R,C),
    retract(grid(R,C)),
    assertz(flagged_grid(R,C)),
    print_grid(),!,
    check_victory().

flag(_,_):-
    print("Invalid Move").

unflag(R,C):-
    flagged_grid(R,C),
    retract(flagged_grid(R,C)),
    assertz(grid(R,C)),
    print_grid(),!,
    check_victory().

unflag(_,_):-
    print("Invalid Move").

check_victory():-
    findall(0,(flagged_grid(R,C), mines(R,C)),GoodList),
    findall(0, flagged_grid(_,_), FullList),
    length(GoodList,GL),
    length(FullList,FL),
    GL = FL,
    GL = 6,
    format("~nCongrats! You Win").
    








