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