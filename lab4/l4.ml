(*Sudoku used by the program*)
let input_sudoku = [|
  [|1;0;0;4|];
  [|0;0;3;0|];
  [|3;0;0;1|];
  [|0;2;0;0|];
|];;

(* Check if there exists another instance of 'value' in the cells that belong in the same
  column that equals 'ycoor'*)
let rec is_same_col sudoku ycoor value cur_row = match cur_row with 
| 4 -> false
| other -> if sudoku.(cur_row).(ycoor) = value then true else is_same_col sudoku ycoor value (cur_row+1);;

(* Check if there exists another instance of 'value' in the cells that belong in the same
  row that equals 'xcoor'*)
let rec is_same_row sudoku xcoor value cur_col = match cur_col with 
| 4 -> false
| other -> if sudoku.(xcoor).(cur_col) = value then true else is_same_row sudoku xcoor value (cur_col+1);;

(* Provides the next coordinates within the 2x2 subgrid for validation *)
let get_new_coords xcoor ycoor cd = 
  (* Emulates the 2bit rotation of '00 10 11 01' to keep the coordinates in the same subgrid*)
let cycle = [| [|(fun x -> x+1); (fun x -> x)|]; [|(fun x -> x); (fun x -> x+1)|] ;
 [|(fun x -> x-1); (fun x -> x)|] ; [|(fun x -> x); (fun x -> x-1)|] |] in
 (* Because the sequence above is not the normal sequence of '0 1 2 3' we have to essentially map where 
 the next part of the sequence will be, and apply the corresponding function to our xcoor & ycoor to get
 the next set of coordinates in the subgrid*)
 let start_cycle = [|0;3;1;2|] in
 let start = ((2*(xcoor mod 2))+(ycoor mod 2)) in
 let current_cycle = (start mod 4) in
 [|(cycle.(start_cycle.(current_cycle)).(0) xcoor);(cycle.(start_cycle.(current_cycle)).(1) ycoor)|];;
 
(* Check if there exists another instance of 'value' in the cells that belong in the same
  quadrant / 2x2 subgrid that ('xcoor','ycoor') belongs to by checking each value in a clockwise manner*)
let rec is_same_quad sudoku xcoor ycoor value countdown = match countdown with
| 4 -> false
| cd -> let new_coords = (get_new_coords xcoor ycoor countdown) in if sudoku.(new_coords.(0)).(new_coords.(1)) = value then true else 
  (is_same_quad sudoku new_coords.(0) new_coords.(1) value (countdown+1));;

(* Checks if 'value' can be added at coordinates ('xcoor','ycoor') without violating any checks*)
let is_valid sudoku xcoor ycoor value = 
  not ((is_same_col sudoku ycoor value 0) 
  || (is_same_row sudoku xcoor value 0) 
  || (is_same_quad sudoku xcoor ycoor value 0));; 

(* Checks if the input sudoku is valid, by attempting to insert each non-zero value in its original place (temporarily replaced with a 0)*)
let rec check_initial_grid sudoku x y =
  let x1 = if y = 4 then (x+1) else x in
  let y1 = if y = 4 then 0 else y in
  let temp = sudoku.(x1).(y1) in
  let temp_sudoku = (Array.copy sudoku) in
  (temp_sudoku.(x1).(y1) <- 0;
if ((x1 = 3) && (y1 = 3)) then true else
  if ((temp = 0) || (is_valid temp_sudoku x1 y1 temp)) then (check_initial_grid sudoku x1 (y1+1)) else false);;

