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

(* Ask the user if they want the car to return to the starting location *)
let ask_return_home () = 
    print_string "Do you want the vehicle to return to the starting location? (y/n): ";
    let response = read_line() in
    if response = "y" then 1.0 else 0.0;;    

(*Attempts to add the next location 'loc' to the next vehicle in the cycle, 
checking if its capacity has been reached. Returns a list containing 2 things: the 
vehicles' locations lists, and a dummy location in a second location list list 
to essentially report back what vehicle had been given this location 'loc' through
 the property 'priority'*)
let rec try_add_loc loc veh_locs cur_num vehicle_count v_capacity = 
  (*Printf.printf "" loc.name cur_num (List.nth v_capacity cur_num); *)
  if is_full veh_locs cur_num (List.nth v_capacity cur_num)
    then try_add_loc loc veh_locs ((cur_num+1) mod vehicle_count) vehicle_count v_capacity
else [add_loc loc veh_locs cur_num; [[{priority=cur_num;xcoor=0.0;ycoor=0.0;name=""}]]];;

(* Distributes the locations among the vehicles by attempting to give the next
location to the vehicle that is after the one that got the last location 
(looping back to the first when reaching the end). Returns the full list of locations
each vehicle gets*)
let rec distribute_locations locs vehicle_count current_number out_list v_capacity = match locs with
| [] -> out_list
| hd :: tl -> (
let output = (try_add_loc hd out_list (current_number mod vehicle_count) vehicle_count 
v_capacity) in (distribute_locations tl vehicle_count ((List.hd(List.hd (List.hd (List.tl output)))).priority+1 mod vehicle_count) (List.hd output)
 v_capacity));; 

 (*Checks to see if the sum of all vehicles' capacites is not less than the
  number of locations*)
 let rec can_it_fit number_locs v_capacity = match v_capacity with
 | [] -> (not (number_locs < 0))
 | hd :: tl -> can_it_fit (number_locs-hd) tl;;


(* Pretty printing *)
let print_loc loc = Printf.printf "Location(priority=%d, x=%f, y=%f, name=%s)\n" loc.priority loc.xcoor loc.ycoor loc.name;;
let print_veh veh = Printf.printf "Vehicle(id=%d, capacity=%d)\n" veh.id veh.capacity ;
                    Printf.printf "|--------------------------|\n";;

(* A quick helper functions *)
let rec sum_ints l s = match l with
| [] -> s
| h::t -> sum_ints t (s+h);;

let get_deliveries veh_locs veh_id = (List.nth veh_locs veh_id);;
let is_veh_full veh_locs veh = (get_deliveries veh_locs veh.id) >= veh.capacity;;

(* Euclidean distance between two points *)
let dist l1 l2 = (((l1.xcoor -. l2.xcoor) ** 2.0) +. ((l1.ycoor -. l2.ycoor) ** 2.0)) ** 0.5;;

(* Distance a car travels, ensure routes are sorted first *)
let rec travel_distance locs sum i go_home =
    if i == ((List.length locs) - 1) then
        (sum +. (dist (List.nth locs 0) {name="HOME"; xcoor=0.; ycoor=0.; priority=0})) +.
        ((dist (List.nth locs i) {name="HOME"; xcoor=0.; ycoor=0.; priority=0}))
    else
        travel_distance locs (sum +. (dist (List.nth locs i) (List.nth locs (i+1)) )) (i + 1) go_home;;

(* Pretty print a vehicles *)
let print_loc loc = Printf.printf "Location(priority=%d, x=%f, y=%f, name=%s)\n" loc.priority loc.xcoor loc.ycoor loc.name;;
let print_veh veh_locs veh =
                    Printf.printf "Vehicle(id=%d, capacity=%d)\n" veh.id veh.capacity ;
                    Printf.printf "|--------------------------|\n";
                    Printf.printf "%f\n" (travel_distance (get_deliveries veh_locs veh.id) 0. 0 true);;

(* Finally we can setup our crud *)
let locations = get_locations();;
let locations = locations_sort locations;;
let vehicles = get_vehicles();;

(* ask if they want the cars to return to the start *)
let return_home = ask_return_home();;

let vehicle_locations = distribute_locations locations (List.length vehicles) 0 (create_out_list (List.length vehicles)) (get_veh_capacities vehicles);;

(* Make sure we have enough space for all deliveries *)
if (List.length locations) <= (sum_ints (get_veh_capacities vehicles) 0) then
    Printf.printf "The fit looks good amigo\n"
else
    (* Ughhnnn there's no way that will fit !~ *)
    exit 1;;

(* Then finally print out the details of each cars delivery run *)
Printf.printf "%d Vehicle routes booked\n" (List.length vehicle_locations);;
Printf.printf "Details of the delivery routes:\n";;
for i = 0 to (List.length vehicles) - 1 do
    Printf.printf "\nVehicle %d route information\n" (i + 1);
    List.iter print_loc (get_deliveries vehicle_locations i);
    Printf.printf "total travel = %f" (travel_distance (get_deliveries vehicle_locations i) 0. 0 return_home);
done;;






