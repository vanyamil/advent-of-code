(*
	Year: 2022
	Day: 14
	Name: Regolith Reservoir
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/14.txt" ;;

(* Main file *)

let every_second (now, acc) el = match now with
	| true -> (false, el :: acc)
	| false -> (true, acc)

let comma_str_to_pair s = 
	match String.split_on_char ',' s with
	| [x; y] -> (int_of_string x, int_of_string y)
	| _ -> failwith "Not a comma-separated pair"

let rec make_a_rock_set accset l = 
	match l with
	| ((x, y) as p1) :: p2 :: t ->
		if p1 = p2
		then make_a_rock_set accset (p2 :: t)
		else
		let unit = V.unit_dist p1 p2 in
		let p1' = V.add p1 unit in
		let accset' = VSet.add p1 accset in
		make_a_rock_set accset' (p1' :: p2 :: t)
	| [p] -> VSet.add p accset
	| [] -> failwith "Should not run on empty list!"

let line_to_rocks accset line =
	line
	|> String.split_on_char ' '
	|> List.fold_left every_second (true, [])
	|> snd
	|> List.map comma_str_to_pair
	|> make_a_rock_set accset

let all_rocks = 
	input_file
	|> Seq.fold_left line_to_rocks VSet.empty

let max_rock_y = 
	VSet.fold (fun (_, y) acc -> max y acc) all_rocks 0

(* Returns settle point of sand, or None if escapes *)
let rec sim_sand rest_at_end rocks sand = 
	(* Arbitrary limit, based on seeing no 200 y *)
	let (_, y) = sand in
	if y > max_rock_y
	then 
		if rest_at_end
		then Some sand
		else None
	else
	let sand' = V.add sand (0, 1) in
	if not (VSet.mem sand' rocks)
	then sim_sand rest_at_end rocks sand'
	else
	let sand' = V.add sand (-1, 1) in
	if not (VSet.mem sand' rocks)
	then sim_sand rest_at_end rocks sand'
	else
	let sand' = V.add sand (1, 1) in
	if not (VSet.mem sand' rocks)
	then sim_sand rest_at_end rocks sand'
	else
	Some sand

let rec sim_sand_total cnt rocks =
	match sim_sand false rocks (500, 0) with
	| None -> cnt
	| Some sand -> sim_sand_total (succ cnt) (VSet.add sand rocks)

let part1 = 
	sim_sand_total 0 all_rocks
;;

let rec sim_sand_with_floor cnt rocks = 
	match sim_sand true rocks (500, 0) with
	| None -> failwith "Should have floor!"
	| Some (500, 0) -> succ cnt (* Count this one too! *)
	| Some sand -> sim_sand_with_floor (succ cnt) (VSet.add sand rocks)

let part2 = 
	sim_sand_with_floor 0 all_rocks
;;

(* Final print *)

print_results (string_of_int part1) (string_of_int part2) ;;
