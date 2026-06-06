(* Question 1 *)


let rec map2_help f l1 l2 = match l1 with
| [] -> [] (*Base case: Reached the end of the list, so return an empty list*)
| _ -> f (List.hd l1) (List.hd l2) :: map2_help f (List.tl l1) (List.tl l2);;
(*If the list is not empty, recursively applies the function to the 1st element 
of each list, and adds it to the output list*)

(* Wrapper function that raises an exception if the list lengths are unequal*)
let map2 f l1 l2 = if (List.length l1) = (List.length l2) then map2_help f l1 l2
 else raise(Failure "Lists must have the same length")

(*Example 1*)
let g = map2_help (fun x y -> x + y) [1;2;3] [4;5;6];;
List.iter (Printf.printf "%d ") g;;
print_endline "";;
(*Example 2*)
let g2 = map2 (fun x y -> x*y*2) [1;2;3] [3;2;1];;
List.iter (Printf.printf "%d ") g2;;
print_endline "";;
(*Example 3*)
(* COMMENTED OUT TO PREVENT PREMATURE EXITING
let g2 = map2 (fun x y -> x*y*2) [1;2;3;4] [3;2;1];;
List.iter (Printf.printf "%d ") g2;;
print_endline "";;*)

(*Question 2*)

(* Returns bool regarding if the number is even *)
let even n =
  n mod 2 = 0

(* Returns a list of all even numbers in the list l*)
let rec filter_even l = match l with
    | [] -> [] (*Base case: Reached the end of the list, so return an empty list*)
    | h::ht -> if even h then h :: filter_even ht else filter_even ht;;
(*If the list is not empty, recursively adds the head to the output list if it's even*)



(*Example 1*)
let f = filter_even [1;2;3;4;5];;
List.iter (Printf.printf "%d ") f;;
print_endline "";;
(*Example 2*)
let f2 = filter_even [1;2;3;4;5;0;0];;
List.iter (Printf.printf "%d ") f2;;
print_endline "";;
(*Example 3*)
let f3 = filter_even [1];;
List.iter (Printf.printf "%d ") f3;;
print_endline "";;


(*Question 3*)
(*Additional composed functions for testing*)
let sixify x = x * 6;;
let sevenify x = x * 7;;

(* Calls two functions back-to-back on a given value 
(effectively written as a lamda)*)
let compose_functions fun1 fun2 = function x -> fun1(fun2(x));;

(*Example 1*)
let composed1 = compose_functions sixify sevenify;;
let () = print_int (composed1 5);;
print_endline "";;

(*Example 2*)
let composed2 = compose_functions (fun x -> x*2) (fun y -> y+3);;
let () = print_int (composed2 5);;
print_endline "";;


(*Question 4*)
let rec reduce f acc lst = match lst with
    | [] -> acc (*Base case: Reached the end of the list, so return the 
accumulator*)
    | h::ht -> reduce f (f acc h) ht;;
(*Otherwise, call reduce again, but use the tail of the list, and replace the 
accumulator value with the function using the current accumulator value and the 
head of the list*)

(*example 1*)
let q1 = reduce (fun x y -> x + y) 0 [1;2;3;4];;
print_int q1;;
print_endline "";;

(*example 2*)
let q2 = reduce (fun x y -> x * y) 1 [1;2;3;4];;
print_int q2;;
print_endline "";;

