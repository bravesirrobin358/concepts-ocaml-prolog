factorial(N,_,'Error: Input must be a non-negative integer'):-
    not(is_of_type(integer,N));
    N<0,!.

factorial(0,Acc,Acc):-!.

factorial(N,Acc,Result):-

    Acc1 is N*Acc,
    N1 is N-1,
    factorial(N1,Acc1,Result).
