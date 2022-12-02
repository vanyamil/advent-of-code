let square x = x * x;;

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
		| End_of_file -> None
	in
	open_in filename
	|> Seq.unfold unfold_fn
	|> Seq.memoize
;;

let file_to_line_list filename = 
	filename
	|> file_to_line_seq 
	|> List.of_seq
;;
