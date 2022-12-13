(*
	Year: 2022
	Day: 12
	Name: Hill Climbing Algorithm
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_list "2022/inputs/12.txt" ;;

(* Main file *)

module V = 
struct
	type t = int * int

	let compare (a: t) (b: t) = Stdlib.compare a b

	let add (x0, y0: t) (x1, y1: t): t = (x0 + x1, y0 + y1)

	let manhattan (x0, y0) (x1, y1) = 
		abs (x1 - x0) + abs (y1 - y0)

	let within (x, y) (l, t) (r, b) = 
		x >= l && x < r && y >= t && y < b

	let to_list (x, y) = [x; y]

	let from_list: int list -> t = function
		| [x; y] -> (x, y)
		| _ -> failwith "List should have two elements!"
end

module PQSet = Set.Make
	(struct
		type t = int * V.t
		let compare = compare
	end)
;;

module VSet = Set.Make (V);;

module Tensor = struct
	type t = {
		shape: int list;
		stride: int list;
		s: string;
	}

	let get coords {shape; stride; s} = 
		let rec aux idx = function
			| [] -> s.[idx]
			| (coord, stride) :: t -> aux (idx + coord * stride) t
		in
		aux 0 (List.map2 (fun a b -> a, b) coords stride)

	let coords c {shape; stride; s} =
		let idx = String.index s c in
		List.map2 (fun shape_el stride_el -> (idx / stride_el) mod shape_el) shape stride

	let create shape s =
		let folder el (hd, tl) = (hd * el, hd :: tl) in
		let (total, stride) = List.fold_right folder shape (1, []) in
		assert (String.length s = total);
		{shape; stride; s}
end

let input_shape = (List.length input_file, String.length @@ List.hd input_file);;

let input_t =
	let shape = V.to_list input_shape in
	input_file
	|> List.fold_left (^) ""
	|> Tensor.create shape

let start_coords = Tensor.coords 'E' input_t |> V.from_list
let end_coords = Tensor.coords 'S' input_t |> V.from_list

let safe_char = function
	| 'S' -> 'a'
	| 'E' -> 'z'
	| c -> c

let dist_opt from_ to_ = 
	if not (V.within to_ (0, 0) input_shape)
	then None
	else 
	let from_ = Tensor.get (V.to_list from_) input_t |> safe_char |> Char.code in
	let to_ = Tensor.get (V.to_list to_) input_t |> safe_char |> Char.code in
	if to_ - from_ < -1
	then None
	else Some 1

let adjust_prio prio pt pq = 
	match PQSet.find_first_opt (fun (_, pt') -> pt' = pt) pq with
	| None -> pq |> PQSet.add (prio, pt)
	| Some (prio', pt') -> 
		if prio' > prio
		then pq |> PQSet.remove (prio', pt') |> PQSet.add (prio, pt)
		else pq

let add_neighbor prio pt pq delta = 
	let pt' = V.add pt delta in
	match dist_opt pt pt' with
	| None -> pq
	| Some d -> 
		let prio' = prio + d - V.manhattan pt end_coords + V.manhattan pt' end_coords in
		adjust_prio prio' pt' pq

let should_stop pt c =
	Tensor.get (V.to_list pt) input_t = c

let rec hillclimb destC seen pq =
	let (priority, pt) as el = PQSet.min_elt pq in
	let pq' = PQSet.remove el pq in
	(* Printf.printf "Looking at %d, %d (d %d)\n" (fst pt) (snd pt) priority; *)
	if VSet.mem pt seen
	then hillclimb destC seen pq'
	else if should_stop pt destC 
	then priority - V.manhattan pt end_coords
	else
		[(0, 1); (1, 0); (0, -1); (-1, 0)]
		|> List.fold_left (add_neighbor priority pt) pq'
		|> hillclimb destC (VSet.add pt seen)

let part1 = 
	let estimate = V.manhattan start_coords end_coords in
	PQSet.empty 
	|> PQSet.add (estimate, start_coords)
	|> hillclimb 'S' VSet.empty
;;

let part2 = 
	let estimate = V.manhattan start_coords end_coords in
	PQSet.empty 
	|> PQSet.add (estimate, start_coords)
	|> hillclimb 'a' VSet.empty
;;

(* Final print *)

print_results (string_of_int part1) (string_of_int part2) ;;
