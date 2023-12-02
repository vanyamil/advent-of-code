(*
	Year: 2023
	Day: 1
	Name: Trebuchet?!
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2023/inputs/1.txt" ;;

(* Main file *)

let code_0 = Char.code '0' ;;
let code_9 = Char.code '9' ;;

let get_digits s = 
	s
	|> String.to_seq
	|> Seq.filter (fun c -> Char.(code c >= code_0 && code c <= code_9))
	|> List.of_seq
	|> List.map Char.escaped
	|> List.map int_of_string

let assemble l =
	(l |> List.hd) * 10 + (l |> List.rev |> List.hd)

let part1 = 
	input_file
	|> Seq.map get_digits
	|> Seq.map assemble
	|> Seq.fold_left (+) 0
;;

let parser = function
	| 0 -> "zero"
	| 1 -> "one"
	| 2 -> "two"
	| 3 -> "three"
	| 4 -> "four"
	| 5 -> "five"
	| 6 -> "six"
	| 7 -> "seven"
	| 8 -> "eight"
	| 9 -> "nine"
	| _ -> ""
;;

let digits = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] ;;

let rec folder acc = function
	(* Base case *)
	| "" -> List.rev acc
	| s -> 
	begin
		let l = String.length s in
		let digit = List.find_opt (fun d -> String.starts_with ~prefix:(string_of_int d) s) digits in
		match digit with
		| Some d -> folder (d :: acc) (String.sub s 1 (l - 1))
		| None -> 
		begin
			let digit = List.find_opt (fun d -> String.starts_with ~prefix:(parser d) s) digits in
			match digit with
			| None -> folder acc (String.sub s 1 (l - 1))
			| Some d -> 
				let l2 = String.length (parser d) in
				folder (d :: acc) (String.sub s l2 (l - l2))
		end
	end
;;

let part2 = 
	input_file
	|> Seq.map (folder [])
	|> Seq.map assemble
	|> Seq.fold_left (+) 0
;;

(* Final print *)

print_results (string_of_int part1) (string_of_int part2) ;;
