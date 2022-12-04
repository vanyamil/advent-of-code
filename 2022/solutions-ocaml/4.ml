(*
	Year: 2022
	Day: 4
	Name: 
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/4.txt" ;;

(* Main file *)

let line_map line =
	let [elf1; elf2] = String.split_on_char ',' line in
	let [r1; r2] = String.split_on_char '-' elf1 |> List.map int_of_string in
	let [r3; r4] = String.split_on_char '-' elf2 |> List.map int_of_string in
	((r1, r2), (r3, r4))
;;

let one_contains_other ((r1, r2), (r3, r4)) =
	(r1 >= r3 && r2 <= r4) ||
	(r3 >= r1 && r4 <= r2)
;;

let part1 = 
	input_file
	|> Seq.map line_map
	|> Seq.filter one_contains_other
	|> Seq.length
;;

let within a l r =
	l <= a && a <= r
;;

let overlap ((r1, r2), (r3, r4)) =
	within r1 r3 r4
	|| within r2 r3 r4
	|| within r3 r1 r2
	|| within r4 r1 r2
;;

let part2 = 
	input_file
	|> Seq.map line_map
	|> Seq.filter overlap
	|> Seq.length
;;

(* Final print *)

print_results part1 part2 ;;
