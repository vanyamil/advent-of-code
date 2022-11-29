let square x = x * x;;

let load_file filename = 
	let ic: in_channel = open_in filename in
	let rec gen_lines acc =
		try
			let line = input_line ic in
			let line = 
				if String.ends_with ~suffix:"\r" line
				then String.sub line 0 (String.length line - 1)
				else line
			in
			gen_lines (line :: acc)
		with
		| End_of_file -> List.rev acc
	in
	gen_lines []
;;
