(*
	Year: 2022
	Day: 1
	Name: Calorie Counting
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_list "2022/inputs/1.txt" ;;

(* Main file *)

let rec sums file_lines cur_acc total_acc =
	if file_lines = []
	then cur_acc :: total_acc
	else 
	let (h :: t) = file_lines in
	if h = ""
	then sums t 0 (cur_acc :: total_acc)
	else
	let v = int_of_string h in
	sums t (v + cur_acc) total_acc
;;

let part1 = 
	sums input_file 0 []
	|> List.fold_left max 0
;;

let rec sublist n acc l =
	if n <= 0
	then acc
	else match l with
	| [] -> List.rev acc
	| h :: t -> sublist (n - 1) (h :: acc) t

let part2 =
	sums input_file 0 []
	|> List.sort compare
	|> List.rev
	|> sublist 3 []
	|> List.fold_left (+) 0
;;

(* Final print *)

print_results part1 part2 ;;
