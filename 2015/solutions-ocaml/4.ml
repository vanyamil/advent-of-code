open Digest;;

let problem_input = "iwrupvqb";;

let get_md5_prefix length in_string =
	let md5 = in_string |> Digest.string |> Digest.to_hex in
	String.sub md5 0 length
;;

let check_cond length idx =
	let combined = problem_input ^ (string_of_int idx) in
	let res = get_md5_prefix length combined in
	res = String.make length '0'
;;

let rec main_loop length idx =
	if check_cond length idx
	then idx
	else main_loop length (idx + 1)
;;

let () =
	print_string "Part 1: ";
	main_loop 5 1 |> print_int;
	print_newline ();
	print_string "Part 2: ";
	main_loop 6 1 |> print_int;
	print_newline ();
;;