type location = {
    priority: int;
    ycoor : float;
    xcoor : float;
    name : string;
};;

type vehicle = {
  id: int;
  capacity: int
};;

(*Functions that prompt for user input of the corresponding type*)
let ask_float question = print_string question; read_float();;
let ask_int question = print_string question; read_int();;

(*Prompts user to enter details for a location*)
let read_location current_number = (
  Printf.printf "Enter details for location: %d\n" current_number;
{
    name=(print_string "Name?: "; read_line());
    priority=(ask_int "Location priority?: ");
    ycoor=(ask_float "Y coordinate: ");
    xcoor=(ask_float "X coordinate: ")
    });;

    
(*Prompts user to enter details for a vehicle*)
let read_vehicle current_number = (
  Printf.printf "Enter detail for vehicle: %d\n" current_number;
  {id=current_number;
  capacity=(ask_int "Vehicle capacity?: ")
  });;

(*Wrapper that handles prompting the user for input details for the provided 
'read_thing' function done 'how_many' times, returning the list of inputed things*)
let rec read_things how_many current_number current_list read_thing =
    print_endline "";
    if how_many > 0 then 
        read_things (how_many-1) (current_number+1) (List.append [read_thing current_number] current_list) read_thing
    else
        current_list;;

(*Sorts the locations by comparing their priority*)
let compare_priority l1 l2 = compare l1.priority l2.priority;;
let locations_sort loc_list = (List.sort compare_priority loc_list);;

(*Multiple uses of 'read_things' to have user input details for locations and 
vehicles respectively*)
let get_locations() = read_things (ask_int "How many delivery locations we doin'?: ") 1 [] read_location;;
let get_vehicles() = read_things (ask_int "How many vehicles we need?: ") 1 [] read_vehicle;;

(*Returns list containing 'vehicle_count' empty lists inside, 1 per vehicle*)
let rec create_out_list vehicle_count = List.init vehicle_count (fun _ -> []);;

(*Gets list of vehicles capacities*)
let rec get_veh_capacities vehicles = match vehicles with
| [] -> []
| hd :: tl -> hd.capacity :: get_veh_capacities tl;;

(*adds the location 'loc' to the location list of vehicles for the vehicle 
chosen (that vehicle is 'cur_num' entries down the list). Ex: if 'cur_num' = 2, then
  it adds to the location list of the 3rd vehicle*)
let rec add_loc loc veh_locs cur_num = match veh_locs with
| [] -> []
| hd :: tl -> if cur_num = 0 then 
    (List.append hd [loc]) :: add_loc loc tl (cur_num-1)
else hd :: add_loc loc tl (cur_num-1);;

(*Checks if the current vehicle's location list has reached max capacity*)
let rec is_full veh_locs cur_num v_capacity = match cur_num with
| 0 -> (List.length (List.hd veh_locs)) = v_capacity
| other -> is_full (List.tl veh_locs) (cur_num-1) v_capacity;;

(*Attempts to add the next location 'loc' to the next vehicle in the cycle, 
checking if its capacity has been reached. Returns a list containing 2 things: the 
vehicles' locations lists, and a dummy location in a second location list list 
to essentially report back what vehicle had been given this location 'loc' through
 the property 'priority'*)
let rec try_add_loc loc veh_locs cur_num vehicle_count v_capacity = 
  Printf.printf "Try %s at v %d with cap %d\n" loc.name cur_num (List.nth v_capacity cur_num); 
  if is_full veh_locs cur_num (List.nth v_capacity cur_num)
    then try_add_loc loc veh_locs ((cur_num+1) mod vehicle_count) vehicle_count v_capacity
else [add_loc loc veh_locs cur_num; [[{priority=cur_num;xcoor=0.0;ycoor=0.0;name=""}]]];;

(* Distributes the locations amond the vehicles by attempting to give the next
location to the vehicle that is after the one that got the last location 
(looping back to the first when reaching the end). Returns the full list of locations
each vehicle gets*)
let rec distribute_locations locs vehicle_count current_number out_list v_capacity = match locs with
| [] -> out_list
| hd :: tl -> (Printf.printf "try add %s\n" hd.name;
let output = (try_add_loc hd out_list (current_number mod vehicle_count) vehicle_count 
v_capacity) in (distribute_locations tl vehicle_count ((List.hd(List.hd (List.hd (List.tl output)))).priority+1 mod vehicle_count) (List.hd output)
 v_capacity));; 

 (*Checks to see if the sum of all vehicles' capacites is not less than the
  number of locations*)
 let rec can_it_fit number_locs v_capacity = match v_capacity with
 | [] -> (not (number_locs < 0))
 | hd :: tl -> can_it_fit (number_locs-hd) tl;;

(*test zone starts here*)
let test_l1 = {priority=1; xcoor=0.0; ycoor=0.0; name="1"};;
let test_l2 = {priority=2; xcoor=0.0; ycoor=0.0; name="2"};;
let test_l3 = {priority=3; xcoor=0.0; ycoor=0.0; name="3"};;
let test_l4 = {priority=4; xcoor=0.0; ycoor=0.0; name="4"};;
let test_l5 = {priority=5; xcoor=0.0; ycoor=0.0; name="5"};;
let test_l6 = {priority=6; xcoor=0.0; ycoor=0.0; name="6"};;

let test_v1 = {id=1; capacity=2};;
let test_v2 = {id=2; capacity=1};;
let test_v3 = {id=3; capacity=3};;

let rec print_vlocs x = match x with
| [] -> []
| hd :: tl -> Printf.printf "%s " hd.name; print_vlocs tl;;

let rec print_locs x = match x with
| [] -> []
| hd :: tl -> ignore (print_vlocs hd); print_endline ""; print_locs tl;;

let loc_list = [test_l6;test_l5;test_l4;test_l3;test_l2;test_l1];;
let x = distribute_locations loc_list 3 0 (create_out_list 3) [2;1;3];;
print_locs x;;
Printf.printf "can %d be distributed among the vehicles: %b" (List.length loc_list) (can_it_fit (List.length loc_list) [2;1;3]);;
(*test zone ends here*)

let main() = 
let locations = get_locations() in
let vehicles = get_vehicles() in
  if can_it_fit (List.length locations) (get_veh_capacities vehicles)
    then let vehicle_locations = distribute_locations locations (List.length vehicles) 0 (create_out_list (List.length vehicles)) (get_veh_capacities vehicles) in
 else (print_endline "The vehicles do not have enough capacity to visit all the locations")

