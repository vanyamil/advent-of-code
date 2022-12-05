(*
	Year: 2022
	Day: 2
	Name: Rock Paper Scissors
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/2.txt" ;;

(* Main file *)

type move =
	| Rock
	| Paper
	| Scissors
;;

let move_to_point = function
	| Rock -> 1
	| Paper -> 2
	| Scissors -> 3
;;

let beating_move = function
	| Rock -> Paper
	| Paper -> Scissors
	| Scissors -> Rock
;;

let score your_move opp_move =
	let outcome = 
		if your_move = beating_move opp_move
		then 6
		else if opp_move = beating_move your_move
		then 0
		else 3
	in
	outcome + move_to_point your_move
;;

let abc_to_move = function
	| "A" -> Rock
	| "B" -> Paper
	| "C" -> Scissors
	| _ -> failwith "Unexpected character"
;;

let xyz_to_move = function
	| "X" -> Rock
	| "Y" -> Paper
	| "Z" -> Scissors
	| _ -> failwith "Unexpected character"
;;

let part1 =
	let process_line acc line =
		match String.split_on_char ' ' line with
		| [opp_move; your_move] ->
			acc + score (xyz_to_move your_move) (abc_to_move opp_move)
		| _ -> failwith "Unexpected line"
	in
	Seq.fold_left process_line 0 input_file
;;

let xyz_to_outcome_move opp_move = function
	| "X" -> opp_move |> beating_move |> beating_move
	| "Y" -> opp_move
	| "Z" -> opp_move |> beating_move
	| _ -> failwith "Unexpected character"
;;

let part2 =
	let process_line acc line =
		match String.split_on_char ' ' line with
		| [opp_move; outcome] ->
			let opp_move = abc_to_move opp_move in
			acc + score (xyz_to_outcome_move opp_move outcome) opp_move
		| _ -> failwith "Unexpected line"
	in
	Seq.fold_left process_line 0 input_file
;;

(* Final print *)

print_results (string_of_int part1) (string_of_int part2) ;;
