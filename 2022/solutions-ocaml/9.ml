(*
	Year: 2022
	Day: 9
	Name: Rope Bridge
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/9.txt" ;;

(* Main file *)

module Coord =
struct
	type t = int * int

	let compare (x0, y0) (x1, y1) =
		match Stdlib.compare x0 x1 with
		| 0 -> Stdlib.compare y0 y1
		| c -> c

	let add (x0, y0) (x1, y1) =
		(x0 + x1, y0 + y1)
end

module CoordSet = Set.Make(Coord)

let dir_to_v = function
	| "U" -> (0, 1)
	| "D" -> (0, -1)
	| "L" -> (-1, 0)
	| "R" -> (1, 0)
	| s -> failwith ("Incorrect direction " ^ s)

(* Go from input file line to direction (a unit vector) and amount *)
let line_to_dir_num s =
	match String.split_on_char ' ' s with
	| [dir; num] ->	(dir_to_v dir, int_of_string num)
	| _ -> failwith ("Could not process input line correctly: " ^ s)

(* From dir vector, amount and start, create a sequence of coords *)
let rec travel_head acc (dir, num) =
	match acc, num with
	| [], _ -> failwith "Why is accumulator empty?"
	| _, 0 -> acc
	| pt :: tail, _ ->
		let pt' = Coord.add pt dir in
		let acc' = pt' :: acc in
		travel_head acc' (dir, pred num)

(* Transform file to sequence of coords for head *)
let head_positions: Coord.t list = 
	input_file
	|> Seq.map line_to_dir_num
	|> Seq.fold_left travel_head [(0, 0)]
	|> List.rev

(* Fold on it, tracking tail and tail visits *)
let advance (hx, hy) (x, y) =
	let dx = hx - x in
	let dy = hy - y in
	match abs dx, abs dy with
	| 0, 0
	| 1, 0
	| 0, 1 
	| 1, 1 -> x, y
	| 2, 0 -> (x + hx) / 2, y
	| 0, 2 -> x, (y + hy) / 2
	| 2, 2 
	| 2, 1
	| 1, 2 -> 
		let (dx', dy') = (dx / abs dx, dy / abs dy) in
		x + dx', y + dy'
	| _ -> failwith "Head is too far from tail!"

let tail_tracker ((pt: Coord.t), set) (head: Coord.t) =
	let pt' = advance head pt in
	(pt', CoordSet.add pt' set)

let rec tail_track_list ((pts: Coord.t list), set) (head: Coord.t) =
	let aux cur_head pt = 
		let pt' = advance cur_head pt in
		pt', pt'
	in
	let (tail, pts') = List.fold_left_map aux head pts in
	(pts', CoordSet.add tail set)

let tail_positions start: CoordSet.t =
	let (_, set) = List.fold_left tail_track_list (start, CoordSet.empty) head_positions in
	set

let sim_rope n =
	List.init n (fun _ -> (0, 0))
	|> tail_positions 
	|> CoordSet.cardinal

let part1 = 
	sim_rope 1
;;

let part2 = 
	sim_rope 9
;;

(* Final print *)

print_results (string_of_int part1) (string_of_int part2) ;;
