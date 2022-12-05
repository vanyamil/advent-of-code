(*
	Year: 2019
	Day: 1
	Name: The Tyranny of the Rocket Equation
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2019/inputs/1.txt" ;;

(* Main file *)

let part1 = 
	input_file
	|> Seq.map int_of_string
	|> Seq.map (fun x -> x / 3 - 2)
	|> Seq.fold_left (+) 0
;;

let rec find_mass x =
	let x' = x / 3 - 2 in
	if x' <= 0
	then 0
	else x' + find_mass x'
;;

let part2 = 
	input_file
	|> Seq.map int_of_string
	|> Seq.map find_mass
	|> Seq.fold_left (+) 0
;;

(* Final print *)

print_results (string_of_int part1) (string_of_int part2) ;;
