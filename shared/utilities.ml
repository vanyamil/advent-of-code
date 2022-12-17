(* Lazy loading a file into a sequence of lines *)
let file_to_line_seq filename : string Seq.t = 
	let unfold_fn ic =
		try
			let line = input_line ic in
			let line = 
				if String.ends_with ~suffix:"\r" line
				then String.sub line 0 (String.length line - 1)
				else line
			in
			Some (line, ic)
		with
		| End_of_file -> (close_in ic; None)
	in
	open_in filename
	|> Seq.unfold unfold_fn
	|> Seq.memoize
;;

(* Loading a file into a list of lines, not lazy *)
let file_to_line_list filename = 
	filename
	|> file_to_line_seq 
	|> List.of_seq
;;

(* Print the results and exit *)
let print_results part1 part2 =
	print_string "Part 1: ";
	print_endline part1;
	print_string "Part 2: ";
	print_endline part2;
	exit 0;
;;

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

	let unit_dist (x0, y0) (x1, y1): t =
		let dx = x1 - x0 in
		let dy = y1 - y0 in
		let ux = 
			if dx = 0
			then 0
			else dx / (abs dx)
		in
		let uy = 
			if dy = 0
			then 0
			else dy / (abs dy)
		in
		(ux, uy)
end

module VSet = Set.Make (V)
