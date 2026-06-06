% Just so i don't have to pass the empty accumulator
sum_odd_numbers(List, Sum) :- sum_odd_numbers(List, 0, Sum).

% Base case sum of empty list is empty %
sum_odd_numbers([], Sum, Sum).

% sum = head + sum(tail) if (head % 2 == 1) else sum(tail)
sum_odd_numbers([Head|Tail], Acc, Sum) :-
      % Ignore even numbers
       0 is Head mod 2 ->  sum_odd_numbers(Tail, Acc, Sum);
       NewAcc is Acc + Head, sum_odd_numbers(Tail, NewAcc, Sum) .