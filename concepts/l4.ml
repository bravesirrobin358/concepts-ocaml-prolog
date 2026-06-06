(*Sudoku usd by the program*)
let input_sudoku = [|
  [|0;0;0;2|];
  [|0;3;4;0|];
  [|0;0;0;0|];
  [|2;0;0;0|];
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

(* Checks if 'value' can be added at coordinates ('xcoor','ycoor') without violating any checks*)(* Checks if 'value' can be added at coordinates ('xcoor','ycoor') without violating any checks *)
let is_valid sudoku xcoor ycoor value = 
  let same_col = is_same_col sudoku ycoor value 0 in
  let same_row = is_same_row sudoku xcoor value 0 in
  let same_quad = is_same_quad sudoku xcoor ycoor value 0 in
  let valid = not (same_col || same_row || same_quad) in  
  valid;;


let rec check_initial_grid sudoku x y =
  let x1 = if y = 4 then (x+1) else x in
  let y1 = if y = 4 then 0 else y in
  let temp = sudoku.(x1).(y1) in
  let temp_sudoku = (Array.copy sudoku) in
  (temp_sudoku.(x1).(y1) <- 0;
if ((x1 = 3) && (y1 = 3)) then true else
  if ((temp = 0) || (is_valid temp_sudoku x1 y1 temp)) then (check_initial_grid sudoku x1 (y1+1)) else false);;


(* pretty printing *)
let print_sud sudoku =Printf.printf "|--------------|\n";
                      Printf.printf " %d | %d | %d | %d \n" sudoku.(0).(0) sudoku.(0).(1) sudoku.(0).(2) sudoku.(0).(3);
                      Printf.printf "|--------------|\n";
                      Printf.printf " %d | %d | %d | %d \n" sudoku.(1).(0) sudoku.(1).(1) sudoku.(1).(2) sudoku.(1).(3);
                      Printf.printf "|--------------|\n";
                      Printf.printf " %d | %d | %d | %d \n" sudoku.(2).(0) sudoku.(2).(1) sudoku.(2).(2) sudoku.(2).(3);
                      Printf.printf "|--------------|\n";
                      Printf.printf " %d | %d | %d | %d \n" sudoku.(3).(0) sudoku.(3).(1) sudoku.(3).(2) sudoku.(3).(3);
                      Printf.printf "|--------------|\n";;

let merge_sudoku initial_sudoku solved_sudoku =
  let result_sudoku = Array.make_matrix 4 4 0 in  (* create a new 4x4 array *)
  for i = 0 to Array.length initial_sudoku - 1 do
    for j = 0 to Array.length initial_sudoku.(i) - 1 do
      if initial_sudoku.(i).(j) = 0 then
        result_sudoku.(i).(j) <- solved_sudoku.(i).(j)  (* use value from solved_sudoku *)
      else
        result_sudoku.(i).(j) <- initial_sudoku.(i).(j)  (* keep value from initial_sudoku *)
    done
  done;
  result_sudoku
;;
let rec find_empty_positions sudoku x y =
  if x = 4 then []
  else 
    if y = 4 then find_empty_positions sudoku (x+1) 0
    else
      if sudoku.(x).(y) = 0 then (x, y) :: find_empty_positions sudoku x (y+1)
      else find_empty_positions sudoku x (y+1)
;;


(*function that tries placing the values 1 to 4 in the cell and checks if valid*)
let rec try_values sudoku x y value=
  if x = 4 then false  (* If we have tried all values, return false *)
  else
    if is_valid sudoku x y value then  (* the value is valid *)
      (sudoku.(x).(y) <- value; true)  (* place the value in the cell and return true *)
    else try_values sudoku x y (value+1)  (* try the next value *)



(* From Wiki: A brute force algorithm visits the empty cells in some order, 
filling in digits sequentially, or backtracking when the number is found to be not valid.
Briefly, a program would solve a puzzle by placing the digit "1" in the first cell and 
checking if it is allowed to be there. If there are no violations (checking row, column, and box constraints)
then the algorithm advances to the next cell and places a "1" in that cell. 
When checking for violations, if it is discovered that the "1" is not allowed, 
the value is advanced to "2". If a cell is discovered where none of the 4 digits is allowed, 
then the algorithm leaves that cell blank and moves back to the previous cell. 
The value in that cell is then incremented by one. This is repeated until the allowed value in the last (81st) cell is discovered.
*)

let rec solve_sudoku temp_sudoku unchanged_sudoku empty_cells index =
  (* first find the empty cell starting at the start position provided *)
  let (x, y) = List.nth empty_cells index in
  (* try placing the values within the cell *)
  if try_values temp_sudoku x y temp_sudoku.(x).(y) then
    (* if it works out then continue to the next cell *)
      (if index = (List.length empty_cells) - 1 then (
        (*copy the temp_sudoku on the unchanged_sudoku*)
        Array.blit temp_sudoku 0 unchanged_sudoku 0 (Array.length temp_sudoku);
        true;
        ) (*TODO: and copy the changed array to the permentate one*)
      else solve_sudoku temp_sudoku unchanged_sudoku empty_cells (index+1))
  else  
      (*if it does not work, then deincrement the index and just recall the try values function which will*)
      (temp_sudoku.(x).(y) <- 0; solve_sudoku temp_sudoku unchanged_sudoku empty_cells (index-1))
;;

(* validates if the solved sudoku is actually correct*)
let rec search_for_5 sudoku x y =
  if x = 4 then false
  else
    if y = 4 then search_for_5 sudoku (x+1) 0 
    else
      if sudoku.(x).(y) = 5 then true
      else search_for_5 sudoku x (y+1);;

(* Print the initial sudoku *)

Printf.printf "Initial Sudoku:\n";
print_sud input_sudoku;;

(* Create a deep copy of the input_sudoku *)
let deep_copy_sudoku sudoku =
  Array.map Array.copy sudoku;;

let sudoku_copy = deep_copy_sudoku input_sudoku;;

let sudoku_combined = deep_copy_sudoku input_sudoku;;

(* putting it all together *)
let main =
  if check_initial_grid sudoku_copy 0 0 then
    let empty_cells = find_empty_positions input_sudoku 0 0 in
    Printf.printf "Empty cells: %d\n" (List.length empty_cells);
    Printf.printf "Empty cells: ";
    List.iter (fun (x, y) -> Printf.printf "(%d, %d) " x y) empty_cells;
    Printf.printf "\n";
    if solve_sudoku sudoku_combined input_sudoku empty_cells 0 then
      if search_for_5 sudoku_combined 0 0 then
        Printf.printf "No solution found!\n"
      else (Printf.printf "Solved Sudoku:\n";
      print_sud sudoku_combined;)
    else
      Printf.printf "No solution found!\n"
  else
    Printf.printf "Invalid initial grid\n";;

main;;